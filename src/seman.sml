structure seman :> seman = struct

  open ast
  open typ
  open entry
  open translate
  open table
  open topsort
  open util
  open printtyp
  open error

  type expty = {exp: exp, ty: Tipo}

  type venv = (string, EnvEntry) Tabla
  type tenv = (string, Tipo) Tabla

  val tabTipos : (string, Tipo) Tabla = tabInserList (tabNueva(), [
    ("int", TInt), 
    ("string", TString)
  ])

  val tabVars : (string, EnvEntry) Tabla = tabInserList (tabNueva(), [
    ("print", Func {level=mainLevel, label="print",
      formals=[TString], result=TUnit, extern=true}),
    ("flush", Func{level=mainLevel, label="flush",
      formals=[], result=TUnit, extern=true}),
    ("getchar", Func{level=mainLevel, label="getstr",
      formals=[], result=TString, extern=true}),
    ("ord", Func{level=mainLevel, label="ord",
      formals=[TString], result=TInt, extern=true}),
    ("chr", Func{level=mainLevel, label="chr",
      formals=[TInt], result=TString, extern=true}),
    ("size", Func{level=mainLevel, label="size",
      formals=[TString], result=TInt, extern=true}),
    ("substring", Func{level=mainLevel, label="substring",
      formals=[TString, TInt, TInt], result=TString, extern=true}),
    ("concat", Func{level=mainLevel, label="concat",
      formals=[TString, TString], result=TString, extern=true}),
    ("not", Func{level=mainLevel, label="not",
      formals=[TInt], result=TInt, extern=true}),
    ("exit", Func{level=mainLevel, label="exit",
      formals=[TInt], result=TUnit, extern=true})
  ])

  fun tiposIguales (TRecord _) TNil = true
    | tiposIguales TNil (TRecord _) = true 
    | tiposIguales TNil TNil = false
    | tiposIguales (TRecord (_, u1)) (TRecord (_, u2)) = (u1=u2)
    | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
    | tiposIguales (TTipo _) _ = raise Fail ("No debería ocurrir! (1)\n")
    | tiposIguales _ (TTipo _) = raise Fail ("No debería ocurrir! (1)\n")
    | tiposIguales a b = (a=b)

  (* transExp : venv * tenv -> ast.exp -> expty *)
  fun transExp (venv, tenv) =
    let

      (* traducción de expresiones *)
      fun trexp (VarExp v) = trvar v
        | trexp (UnitExp _) = {exp=SCAF, ty=TUnit}
        | trexp (NilExp _) = {exp=SCAF, ty=TNil}
        | trexp (IntExp (i, _)) = {exp=SCAF, ty=TInt}
        | trexp (StringExp (s, _)) = {exp=SCAF, ty=TString}
        | trexp (CallExp ({func, args}, nl)) =
            let
              (* traduce cada expresión de args *)
              val targs = map trexp args
              
              (* busca la función en la tabla *)
              val (formals, result) = case tabBusca (func, venv) of
                NONE => error ("no existe la función \""^func^"\"", nl)
                | SOME (Func {level,label,formals,result,extern}) => (formals, result)
                | _ => error ("se esperaba que \""^func^"\" fuese una función", nl)
              
              (* verifica número de argumentos *)
              val l = if length targs < length formals then
                        error ("faltan argumentos en la llamada a \""^func^"\"", nl)
                      else if length targs > length formals then
                        error ("sobran argumentos en la llamada a \""^func^"\"", nl)
                      else
                        ListPair.zip (formals, targs)
              
              (* verifica tipo de cada argumento *)
              val r = List.map (fn (t, {exp,ty}) => tiposIguales t ty) l
              val _ = if List.all (fn b => b) r then () else
                        error ("tipo incorrecto en arg "^(boolsToStr r)^
                               " de la llamada a \""^func^"\"", nl)
            in
              {exp=SCAF, ty=result}
            end
        | trexp (OpExp ({left, oper=EqOp, right}, nl)) =
            let
              val {exp=_, ty=tyl} = trexp left
              val {exp=_, ty=tyr} = trexp right
            in
              if tiposIguales tyl tyr 
                    andalso not (tyl=TNil andalso tyr=TNil) 
                    andalso tyl<>TUnit 
                then {exp=SCAF, ty=TInt}
                else error ("tipos no comparables por igualdad", nl)
            end
        | trexp (OpExp ({left, oper=NeqOp, right}, nl)) = 
            let
              val {exp=_, ty=tyl} = trexp left
              val {exp=_, ty=tyr} = trexp right
            in
              if tiposIguales tyl tyr 
                    andalso not (tyl=TNil andalso tyr=TNil) 
                    andalso tyl<>TUnit 
                then {exp=SCAF, ty=TInt}
                else error ("tipos no comparables por desigualdad", nl)
            end
        | trexp (OpExp ({left, oper, right}, nl)) = 
            let
              val {exp=_, ty=tyl} = trexp left
              val {exp=_, ty=tyr} = trexp right
            in
              if tiposIguales tyl tyr then
                case oper of
                  PlusOp     => if tyl=TInt then {exp=SCAF,ty=TInt} else 
                                  error("error de tipos en +", nl)
                  | MinusOp  => if tyl=TInt then {exp=SCAF,ty=TInt} else 
                                  error("error de tipos en -", nl)
                  | TimesOp  => if tyl=TInt then {exp=SCAF,ty=TInt} else 
                                  error("error de tipos en *", nl)
                  | DivideOp => if tyl=TInt then {exp=SCAF,ty=TInt} else 
                                  error("error de tipos en /", nl)
                  | LtOp     => if tyl=TInt orelse tyl=TString then {exp=SCAF,ty=TInt} else 
                                  error("error de tipos en <", nl)
                  | LeOp     => if tyl=TInt orelse tyl=TString then {exp=SCAF,ty=TInt} else 
                                  error("error de tipos en <=", nl)
                  | GtOp     => if tyl=TInt orelse tyl=TString then {exp=SCAF,ty=TInt} else 
                                  error("error de tipos en >", nl)
                  | GeOp     => if tyl=TInt orelse tyl=TString then {exp=SCAF,ty=TInt} else 
                                  error("error de tipos en >=", nl)
                  | _        => raise Fail "No debería ocurrir! (2)\n"
              else error("error de tipos", nl)
            end
        | trexp (RecordExp ({fields, typ}, nl)) =
            let
              (* traduce cada expresión de fields *)
              val tfields = map (fn (sy, ex) => (sy, trexp ex)) fields
            
              (* busca el tipo del record en la tabla *)
              val (tyr, cs) = case tabBusca (typ, tenv) of
                NONE => error ("tipo inexistente \""^typ^"\"", nl)
                | SOME (TRecord (cs, u)) => (TRecord (cs, u), cs)
                | _ => error ("el tipo \""^typ^"\" no es record", nl)
            
              (* verifica que cada campo esté en orden y tenga el tipo correcto *)
              fun verificar [] [] = ()
                | verificar (c::cs) [] = error ("faltan campos en el record", nl)
                | verificar [] (c::cs) = error("sobran campos en el record", nl)
                | verificar ((s,t)::cs) ((sy,{exp,ty})::ds) =
                  if s<>sy then 
                    error("error de campo \""^sy^"\" en el record", nl)
                  else if not (tiposIguales (!t) ty) then
                    error("error de tipo en el campo \""^s^"\" del record", nl)
                  else
                    verificar cs ds
              val _ = verificar cs tfields
            in
              {exp=SCAF, ty=tyr}
            end
        | trexp (SeqExp (s, nl)) =
            let
              val lexti = map trexp s
              val exprs = map (fn {exp, ty} => exp) lexti
              val {exp, ty=tipo} = hd (rev lexti)
            in
              {exp=SCAF, ty=tipo}
            end
        | trexp (AssignExp ({var, exp}, nl)) =
            let
              (* verifica que la variable no sea RO *)
              val _ = case var of
                SimpleVar s => (case tabBusca (s, venv) of
                  SOME VIntRO => error ("asignación de variable índice de for \""^s^"\"", nl)
                  | _ => ())
                | _ => ()
                  
              val {exp=vexp,ty=vty} = trvar (var, nl)
              val {exp, ty} = trexp exp
              val _ = if tiposIguales vty ty then () else
                        error ("error de tipos en asignación", nl)
            in
              {exp=SCAF, ty=TUnit}
            end
        | trexp (IfExp ({test, then', else'=SOME else'}, nl)) =
            let 
              val {exp=testexp, ty=tytest} = trexp test
              val {exp=thenexp, ty=tythen} = trexp then'
              val {exp=elseexp, ty=tyelse} = trexp else'
            in
              if tytest<>TInt then
                error ("error de tipo en la condición", nl)
              else if not (tiposIguales tythen tyelse) then
                error ("los tipos del then y else no coinciden", nl)
              else
                {exp=SCAF, ty=tythen}
            end
        | trexp (IfExp ({test, then', else'=NONE}, nl)) =
            let
              val {exp=exptest, ty=tytest} = trexp test
              val {exp=expthen, ty=tythen} = trexp then'
            in
              if tytest<>TInt then
                error ("error de tipo en la condición", nl)
              else if tythen<>TUnit then
                error ("el cuerpo del then no puede devolver valor", nl)
              else
                {exp=SCAF, ty=TUnit}
            end
        | trexp (WhileExp ({test, body}, nl)) =
            let
              val {exp=exptest, ty=tytest} = trexp test
              (* val _ = preWhileFor() *)
              val {exp=expbody, ty=tybody} = trexp body
              (* val _ = postWhileFor() *)
            in
              if tytest<>TInt then
                error ("error de tipo en la condición", nl)
              else if tybody<>TUnit then
                error ("el cuerpo del while no puede devolver valor", nl)
              else
                {exp=SCAF, ty=TUnit}
            end
        | trexp (ForExp ({var, escape, lo, hi, body}, nl)) =
            let
              val venv' = tabInserta (var, VIntRO, venv)
              val {exp=explo, ty=tylo} = trexp lo
              val {exp=exphi, ty=tyhi} = trexp hi
              (* val _ = preWhileFor() *)
              val {exp=expbody, ty=tybody} = transExp (venv', tenv) body
              (* val _ = preWhileFor() *)
            in
              if tylo<>TInt then
                error ("error en el tipo de lo", nl)
              else if tyhi<>TInt then 
                error ("error en el tipo de hi", nl)
              else if tybody<>TUnit then
                error ("el cuerpo del for no puede devolver valor", nl)
              else
                {exp=SCAF, ty=TUnit}
            end
        | trexp (LetExp ({decs, body}, _)) =
            let
              val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => transDec (v, t) d) 
                                        (venv, tenv, []) decs
              val {exp=expbody, ty=tybody} = transExp (venv', tenv') body
            in 
              {exp=SCAF, ty=tybody}
            end
        | trexp (BreakExp nl) =
            {exp=SCAF, ty=TUnit} (*COMPLETAR*)
        | trexp (ArrayExp ({typ, size, init}, nl)) =
            let
              (* traduce las expresiónes *)
              val {exp=expsize, ty=tysize} = trexp size
              val {exp=expinit, ty=tyinit} = trexp init
              val (tyresult, tycontent) = case tabBusca (typ, tenv) of
                NONE => error ("tipo inexistente \""^typ^"\"", nl)
                | SOME (TArray (t, u)) => (TArray (t, u), t)
                | _ => error ("el tipo \""^typ^"\" no es un array", nl)
            in
              if tysize<>TInt then
                error ("error en el tipo de size", nl)
              else if not (tiposIguales tyinit tycontent) then
                error ("tipo de init incorrecto", nl)
              else
                {exp=SCAF, ty=tyresult}
            end

      (* traducción de variables *)
      and trvar (SimpleVar s, nl) =
            let
              val tipo = case tabBusca (s, venv) of
                NONE => error ("variable inexistente \""^s^"\"", nl)
                | SOME VIntRO => TInt
                | SOME (Var {ty}) => ty
                | _ => error ("se esperaba que \""^s^"\" fuese una variable", nl)
            in
              {exp=SCAF, ty=tipo}
            end
        | trvar (FieldVar (v, s), nl) = 
            let
              (* traduce la variable v *)
              val (exp, cs) = case trvar (v, nl) of
                {exp, ty=TRecord (cs, u)} => (exp, cs)
                | _ => error ("se esperaba un tipo record para buscar el campo \""^s^"\"", nl)
              
              (* busca el tipo del campo s en el tipo de la variable v *)
              val tipo = case List.find (fn (sy,_) => s=sy) cs of
                NONE => error ("no existe el campo \""^s^"\" en el record", nl)
                | SOME (_,r) => !r
            in
              {exp=SCAF, ty=tipo}
            end
        | trvar (SubscriptVar (v, e), nl) =
            let
              (* traduce la variable v y la expresión e *)
              val (vexp, tipo) = case trvar (v, nl) of
                {exp, ty=TArray (t, u)} => (exp, t)
                | _ => error ("se esperaba un tipo array", nl)
              val iexp = case trexp e of
                {exp, ty=TInt} => exp
                | _ => error ("el índice debe ser de tipo entero", nl)
            in
              {exp=SCAF, ty=tipo}
            end
    in
      trexp
    end

  (* traducción de declaraciones *)
  and transDec (venv, tenv) =
    let
      fun trdec (VarDec ({name, escape, typ=NONE, init}, nl)) =
            let
              val {exp=expinit, ty=tyinit} = transExp (venv, tenv) init
              val _ = if tyinit<>TUnit andalso tyinit<>TNil then ()
                      else error ("inicialización incorrecta de variable \""^name^"\"", nl)
              val venv' = tabRInserta (name, Var {ty=tyinit}, venv)
            in
              (venv', tenv, [{exp=SCAF, ty=tyinit}])
            end
        | trdec (VarDec ({name, escape, typ=SOME s, init}, nl)) =
            let
              val {exp=expinit, ty=tyinit} = transExp (venv, tenv) init
              val tyvar = case tabBusca (s, tenv) of
                SOME t => t
                | NONE => error ("tipo inexistente \""^s^"\"", nl)
              val _ = if tiposIguales tyvar tyinit then () 
                      else error ("tipo \""^s^"\" no compatible con inicialización", nl)
              val venv' = tabRInserta (name, Var {ty=tyvar}, venv)
            in
              (venv', tenv, [{exp=SCAF, ty=tyvar}]) (*TODO qué es la lista que devuelve trdec*)
            end
        | trdec (TypeDec []) = raise Fail "no debería pasar!"
        | trdec (TypeDec (ts as (_,nl)::_)) =
            let
              val decs = List.map (fn (dec, pos) => dec) ts
              val tenv' = (fijaTipos decs tenv) handle
                Ciclo => error ("ciclo en la declaración de tipos", nl)
                | Fail s => error (s, nl)
            in
              (venv, tenv', [])
            end
        | trdec (FunctionDec fs) =
            let
              (* Verifica que no haya funciones repetidas en el batch *)
              val _ = List.foldl (fn (({name,params,result,body},nl), ns) => 
                if List.exists (fn x => x=name) ns
                  then error ("declaración de tipo \""^name^"\" duplicada", nl)
                  else (name::ns)) [] fs
            
              (* traduce un parámetro de función a su Tipo *)
              fun paramToTipo nl {name,escape,typ} = case tabBusca (typ, tenv) of
                SOME t => t
                | NONE => error ("tipo inexistente \""^typ^"\"", nl)
              
              (* traduce el resultado de la función a su Tipo *)
              fun resultToTipo nl (SOME s) = (case tabBusca (s, tenv) of
                    SOME t => t
                    | NONE => error ("tipo inexistente \""^s^"\"", nl))
                | resultToTipo _ NONE = TUnit
              
              (* traduce una declaración de función en su EnvEntry de tipo Func *)
              fun trfn ({name,params,result,body}, nl) =
                let
                  val _ = (hasDup params) handle Duplicated s => 
                    error ("la función \""^name^"\" tiene el parámetro duplicado \""^s^"\"", nl) 
                  val fs = List.map (paramToTipo nl) params
                  val r = resultToTipo nl result
                in
                  (name, Func {level=(), label=newLabel(), formals=fs, result=r, extern=false})
                end
              
              (* crea nuevo entorno con la declaración de las funciones del batch *)
              val venv' = tabInserList (venv, (List.map trfn fs))
              
              (* traduce un parámetro de función a su EnvEntry de tipo Var *)
              fun paramToVar nl p = (#name(p), Var {ty=paramToTipo nl p})
              
              (* traduce una función y verifica el tipo de retorno *)
              fun trbody ({name,params,result,body}, nl) = 
                let
                  (* crear nuevo entorno con las variables de los parámetros *)
                  val venv'' = tabInserList (venv', List.map (paramToVar nl) params)
                  val {exp,ty} = transExp (venv'', tenv) body
                  val tyresult = resultToTipo nl result
                in
                  if tiposIguales ty tyresult then {exp=exp, ty=ty}
                  else (error ("cuerpo de la función \""^name^"\" incorrecto", nl))
                end
              
              (* traduce todas las funciones del batch *)
              val res = List.map trbody fs
            in
              (venv', tenv, res)
            end
    in
      trdec
    end

  (* chequea tipos y traduce un programa tiger *)
  fun transProg ex =
        let
          val main = LetExp ({
                decs=[FunctionDec[({name="_tigermain", params=[], result=SOME "int", body=ex}, 0)]],
                body=UnitExp 0}, 0)
          val _ = transExp (tabVars, tabTipos) main
        in
          ()
        end

end
