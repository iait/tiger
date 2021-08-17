structure seman :> seman = struct

  open ast
  open tentry
  open ventry
  open trans
  open table
  open topsort
  open util
  open typepp
  open error
  open stack

  type expty = {exp: exp, ty: Tipo}

  type venv = (string, EnvEntry) Tabla
  type tenv = (string, Tipo) Tabla

  val tabTipos : (string, Tipo) Tabla = tabInserList (tabNueva(), [
    ("int", TInt), 
    ("string", TString)
  ])
  
  val levelPila: level Pila = nuevaPila1 outermost
  fun pushLevel l = pushPila levelPila l
  fun popLevel() = popPila levelPila
  fun topLevel() = topPila levelPila

  val tabVars : (string, EnvEntry) Tabla = tabInserList (tabNueva(), [
    ("print", Func {level=topLevel(), label="print",
      formals=[TString], result=TUnit, extern=true}),
    ("flush", Func{level=topLevel(), label="flush",
      formals=[], result=TUnit, extern=true}),
    ("getchar", Func{level=topLevel(), label="getstr",
      formals=[], result=TString, extern=true}),
    ("ord", Func{level=topLevel(), label="ord",
      formals=[TString], result=TInt, extern=true}),
    ("chr", Func{level=topLevel(), label="chr",
      formals=[TInt], result=TString, extern=true}),
    ("size", Func{level=topLevel(), label="size",
      formals=[TString], result=TInt, extern=true}),
    ("substring", Func{level=topLevel(), label="substring",
      formals=[TString, TInt, TInt], result=TString, extern=true}),
    ("concat", Func{level=topLevel(), label="concat",
      formals=[TString, TString], result=TString, extern=true}),
    ("not", Func{level=topLevel(), label="not",
      formals=[TInt], result=TInt, extern=true}),
    ("exit", Func{level=topLevel(), label="exit",
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
(******** variables ********)
      fun trexp (VarExp v) = trvar v

(******** constantes ********)
        | trexp (UnitExp _) = {exp=unitExp(), ty=TUnit}
        | trexp (NilExp _) = {exp=nilExp(), ty=TNil}
        | trexp (IntExp (i, _)) = {exp=intExp i, ty=TInt}
        | trexp (StringExp (s, _)) = {exp=stringExp s, ty=TString}

(******** llamada a función ********)
        | trexp (CallExp ({func, args}, nl)) =
            let
              (* traduce cada expresión de args *)
              val targs = map trexp args
              
              (* busca la función en la tabla *)
              val (lev,name,formals,result,extern) = 
                case tabBusca (func, venv) of
                  NONE => error ("no existe la función \""^func^"\"", nl)
                  | SOME (Func {level,label,formals,result,extern}) => 
                      (level,label,formals,result,extern)
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
              
              (* expresiones de los argumentos *)
              val args = map (fn {exp,ty} => exp) targs
              
              (* crea expresión de la llamada *)
              val exp = callExp {
                name = name,
                extern = extern,
                proc = (result=TUnit),
                lev = lev,
                args = args
              }
            in
              {exp=exp, ty=result}
            end

(******** operación binaria ********)
        | trexp (OpExp ({left, oper, right}, nl)) =
            let
              val {exp=expl, ty=tyl} = trexp left
              val {exp=expr, ty=tyr} = trexp right
              fun checkEq oper = case oper of
                EqOp => true
                | NeqOp => true
                | _ => false
              fun checkRel oper = case oper of
                LtOp => true
                | LeOp => true
                | GtOp => true
                | GeOp => true
                | _ => false
              val exp =
                if checkEq oper andalso tyl=TNil andalso tyr=TNil 
                  then error ("comparación nils", nl) 
                else if not (tiposIguales tyl tyr) then error ("tipos distintos", nl)
                else if checkEq oper then
                  if tyl=TNil then nilCompare {record=expr, oper=oper}
                  else if tyr=TNil then nilCompare {record=expl, oper=oper}
                  else if tyl=TInt then binOpIntRelExp {left=expl, oper=oper, right=expr}
                  else if tyl=TString then binOpStrExp {left=expl, oper=oper, right=expr}
                  else error ("tipos no comparables por igualdad", nl)
                else if checkRel oper then
                  if tyl=TInt then binOpIntRelExp {left=expl, oper=oper, right=expr}
                  else if tyl=TString then binOpStrExp {left=expl, oper=oper, right=expr}
                  else error ("tipos no comparables", nl)
                else
                  if tyl=TInt then binOpIntExp {left=expl, oper=oper, right=expr}
                  else error ("tipos no aritméticos", nl)
              in
                {exp=exp, ty=TInt}
              end

(******** inicialización de record ********)
        | trexp (RecordExp ({fields, typ}, nl)) =
            let
              (* traduce cada expresión de fields *)
              (* tfields : (symbol * expty) list *)
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
                | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
                  if s<>sy then 
                    error("error de campo \""^sy^"\" en el record", nl)
                  else if not (tiposIguales (!t) ty) then
                    error("error de tipo en el campo \""^s^"\" del record", nl)
                  else
                    verificar cs ds
              val _ = verificar cs tfields
              
              (* crea la expresión de inicialización del record *)
              val es = map (fn (_, {exp, ty}) => exp) tfields
              val is = List.tabulate (length es, (fn x => x))
              val exp = recordExp (ListPair.zip (es, is))
            in
              {exp=exp, ty=tyr}
            end

(******** inicialización de array ********)
        | trexp (ArrayExp ({typ, size, init}, nl)) =
            let
              (* traduce las expresiónes *)
              val {exp=expsize, ty=tysize} = trexp size
              val {exp=expinit, ty=tyinit} = trexp init
              val (tyresult, tycontent) = case tabBusca (typ, tenv) of
                NONE => error ("tipo inexistente \""^typ^"\"", nl)
                | SOME (TArray (t, u)) => (TArray (t, u), t)
                | _ => error ("el tipo \""^typ^"\" no es un array", nl)
              val exp =
                if tysize<>TInt then
                  error ("error en el tipo de size", nl)
                else if not (tiposIguales tyinit tycontent) then
                  error ("tipo de init incorrecto", nl)
                else
                  arrayExp {size=expsize, init=expinit}
            in
                  {exp=exp, ty=tyresult}
            end

(******** secuencia ********)
        | trexp (SeqExp (s, nl)) =
            let
              val lexti = map trexp s
              val exprs = map (fn {exp, ty} => exp) lexti
              val {exp=_, ty=tipo} = List.last lexti
              val exp = seqExp exprs
            in
              {exp=exp, ty=tipo}
            end

(******** asignación ********)
        | trexp (AssignExp ({var, exp}, nl)) =
            let
              (* verifica que la variable no sea RO *)
              val _ = case var of
                SimpleVar s => (case tabBusca (s, venv) of
                  SOME (VIntRO _) => error ("asignación de variable índice de for \""^s^"\"", nl)
                  | _ => ())
                | _ => ()
                  
              val {exp=lexp,ty=lty} = trvar (var, nl)
              val {exp=rexp, ty=rty} = trexp exp
              val _ = if tiposIguales lty rty then () else
                        error ("error de tipos en asignación", nl)
              val exp = assignExp {var=lexp, exp=rexp}
            in
              {exp=exp, ty=TUnit}
            end

(******** ifthen ********)
        | trexp (IfExp ({test, then', else'=NONE}, nl)) =
            let
              val {exp=exptest, ty=tytest} = trexp test
              val {exp=expthen, ty=tythen} = trexp then'
              val exp =
                if tytest<>TInt then
                  error ("error de tipo en la condición", nl)
                else if tythen<>TUnit then
                  error ("el cuerpo del then no puede devolver valor", nl)
                else
                  ifThenExp {test=exptest, then'=expthen}
            in
              {exp=exp, ty=TUnit}
            end

(******** ifthenelse ********)
        | trexp (IfExp ({test, then', else'=SOME else'}, nl)) =
            let 
              val {exp=exptest, ty=tytest} = trexp test
              val {exp=expthen, ty=tythen} = trexp then'
              val {exp=expelse, ty=tyelse} = trexp else'
              val exp = 
                if tytest<>TInt then
                  error ("error de tipo en la condición", nl)
                else if not (tiposIguales tythen tyelse) then
                  error ("los tipos del then y else no coinciden", nl)
                else if tythen=TUnit then
                  ifThenElseExpUnit {test=exptest, then'=expthen, else'=expelse}
                else
                  ifThenElseExp {test=exptest, then'=expthen, else'=expelse}
            in
              {exp=exp, ty=tythen}
            end

(******** ciclo while ********)
        | trexp (WhileExp ({test, body}, nl)) =
            let
              val {exp=exptest, ty=tytest} = trexp test
              val _ = preLoopExp()
              val {exp=expbody, ty=tybody} = trexp body
              val exp = 
                if tytest<>TInt then
                  error ("error de tipo en la condición", nl)
                else if tybody<>TUnit then
                  error ("el cuerpo del while no puede devolver valor", nl)
                else
                  whileExp {test=exptest, body=expbody}
              val _ = postLoopExp()
            in
              {exp=exp, ty=TUnit}
            end

(******** ciclo for ********)
        | trexp (ForExp ({var, escape, lo, hi, body}, nl)) =
            let
            
              val acc = allocLocal (topLevel()) (!escape)
              val entry = VIntRO {access = acc, level = getActualLev()}
              val venv' = tabRInserta (var, entry, venv)
              val expvar = varDec acc
              val {exp=explo, ty=tylo} = trexp lo
              val {exp=exphi, ty=tyhi} = trexp hi
              val _ = preLoopExp()
              val {exp=expbody, ty=tybody} = transExp (venv', tenv) body
              val exp =
                if tylo<>TInt then
                  error ("error en el tipo de lo", nl)
                else if tyhi<>TInt then 
                  error ("error en el tipo de hi", nl)
                else if tybody<>TUnit then
                  error ("el cuerpo del for no puede devolver valor", nl)
                else
                  forExp {lo=explo, hi=exphi, var=expvar, body=expbody}
              val _ = postLoopExp()
            in
                {exp=exp, ty=TUnit}
            end

(******** break ********)
        | trexp (BreakExp nl) =
            let
              val exp = breakExp() handle Break => error ("break incorrecto", nl)
            in
              {exp=exp, ty=TUnit}
            end

(******** let ********)
        | trexp (LetExp ({decs, body}, _)) =
            let
              (* Recibe declaración, entorno de valores y tipos, y lista de expresiones.
               * Retorna entornos aumentados por transDec y crea una nueva lista de 
               * expresiones con las anteriores y las nuevas. *)
              fun aux (dec, (venv, tenv, exps)) = 
                let
                  val (venv', tenv', exps') = transDec (venv, tenv) dec
                in
                  (venv', tenv', exps @ exps')
                end
              val (venv', tenv', expdecs) = List.foldl aux (venv, tenv, []) decs
              val {exp=expbody, ty=tybody} = transExp (venv', tenv') body
              val exp = letExp (expdecs, expbody)
            in 
              {exp=exp, ty=tybody}
            end

      (* traducción de variables *)

(******** traduce variable simple ********)
      and trvar (SimpleVar s, nl) =
            let
              val (tipo, acc, decLev) = case tabBusca (s, venv) of
                NONE => error ("variable inexistente \""^s^"\"", nl)
                | SOME (VIntRO {access,level}) => (TInt, access, level)
                | SOME (Var {ty,access,level}) => (ty, access, level)
                | _ => error ("se esperaba que \""^s^"\" fuese una variable", nl)
              val exp = simpleVar (acc, decLev)
            in
              {exp=exp, ty=tipo}
            end

(******** traduce variable de acceso a campo de record ********)
        | trvar (FieldVar (v, s), nl) = 
            let
              (* traduce la variable v *)
              val (vexp, cs) = case trvar (v, nl) of
                {exp, ty=TRecord (cs, u)} => (exp, cs)
                | _ => error ("se esperaba un tipo record para buscar el campo \""^s^"\"", nl)
              
              (* busca el tipo del campo s en el tipo de la variable v *)
              val (tipo, i) = case List.find (fn (sy,_,_) => s=sy) cs of
                NONE => error ("no existe el campo \""^s^"\" en el record", nl)
                | SOME (_,r,i) => (!r, i)
              
              (* genera expresión *)
              val exp = fieldVar (vexp, i)
            in
              {exp=exp, ty=tipo}
            end

(******** traduce variable de subíndice de array ********)
        | trvar (SubscriptVar (v, e), nl) =
            let
              (* traduce la variable v y la expresión e *)
              val (vexp, tipo) = case trvar (v, nl) of
                {exp, ty=TArray (t, u)} => (exp, t)
                | _ => error ("se esperaba un tipo array", nl)
              val iexp = case trexp e of
                {exp, ty=TInt} => exp
                | _ => error ("el índice debe ser de tipo entero", nl)
              (* genera expresión *)
              val exp = subscriptVar (vexp, iexp)
            in
              {exp=exp, ty=tipo}
            end
    in
      trexp
    end

  (* traducción de declaraciones *)

  (* transDec : (venv * tenv) -> dec -> (venv * tenv * trans.exp list) *)
  and transDec (venv, tenv) =
    let
(******** traduce declaración de variable ********)
      fun trdec (VarDec ({name, escape, typ, init}, nl)) =
            let
              val {exp=expinit, ty=tyinit} = transExp (venv, tenv) init
              val ty = case typ of
                NONE => if tyinit<>TUnit andalso tyinit<>TNil then tyinit
                        else error ("inicialización incorrecta de variable \""^name^"\"", nl)
                | SOME s => (case tabBusca (s, tenv) of
                    NONE => error ("tipo inexistente \""^s^"\"", nl)
                    | SOME tyvar => if tiposIguales tyvar tyinit then tyvar 
                                    else error ("tipo \""^s^"\" no compatible con init", nl))
              val acc = allocLocal (topLevel()) (!escape)
              val entry = Var {
                ty = ty,
                access = acc,
                level = getActualLev()
              }
              val venv' = tabRInserta (name, entry, venv)
              val exp = assignExp {var = (varDec acc), exp = expinit} 
            in
              (venv', tenv, [exp])
            end

(******** traduce declaración de tipos ********)
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

(******** traduce declaración de funciones ********)
        | trdec (FunctionDec []) = raise Fail "no debería pasar!"
        | trdec (FunctionDec fs) =
            let
              (* Verifica que no haya funciones repetidas en el batch *)
              val _ = List.foldl (fn (({name,params,result,body},nl), ns) => 
                if List.exists (fn x => x=name) ns
                  then error ("declaración de función \""^name^"\" duplicada", nl)
                  else (name::ns)) [] fs

              (* busca un tipo por nombre y si no lo encuetra lanza excepción *)
              fun findTipo nl s = case tabBusca (s, tenv) of
                SOME t => t
                | NONE => error ("tipo inexistente \""^s^"\"", nl)

              (* obtiene el tipo de retorno *)
              fun tipoRetorno nl result = Option.getOpt (Option.map (findTipo nl) result, TUnit)

              (* traduce una declaración de función en su EnvEntry de tipo Func *)
              fun trfn ({name,params,result,body}, nl) =
                let
                  val _ = (hasDup params) handle Duplicated s => 
                    error ("la función \""^name^"\" tiene el parámetro duplicado \""^s^"\"", nl) 
                  val fs = List.map (fn p => findTipo nl (#typ p)) params
                  val r = tipoRetorno nl result
                  val formals = List.map (fn f => !(#escape(f))) params
                  (* crea nuevo level *)
                  val label = if name="_tigermain" then "_tigermain" else newLabel()
                  val lev = newLevel {parent=topLevel(), name=label, formals=formals}
                  val entry = Func {
                    level=lev, label=label, formals=fs, result=r, extern=false}
                in
                  (name, entry)
                end

              (* crea nuevo entorno con la declaración de las funciones del batch *)
              val entries = List.map trfn fs
              val venv' = tabInserList (venv, entries)

              (* extrae los levels de las entries *)
              fun extractLevel (_, Func {level,label,formals,result,extern}) = level
                | extractLevel _ = raise Fail "solo Func"
              val levels = List.map extractLevel entries

              (* traduce un parámetro de función a su EnvEntry de tipo Var *)
              fun paramToVar nl ({name,escape,typ}, acc) = 
                (name, Var {
                  ty=findTipo nl typ,
                  access=acc,
                  level=getActualLev()})

              (* traduce una función y verifica el tipo de retorno *)
              fun trbody (({name,params,result,body}, nl), lev) = 
                let
                  (* pre *)
                  val _ = (preFunctionDec(); pushLevel lev)
                  (* crear nuevo entorno con las variables de los parámetros *)
                  val accs = formals (topLevel())
                  val args = List.map (paramToVar nl) (ListPair.zip (params, accs))
                  val venv'' = tabInserList (venv', args)
                  (* traduce la expresión del body de la función *)
                  val {exp=expbody, ty=tybody} = transExp (venv'', tenv) body
                  (*val DEBUG = printTransExp expbody*)
                  (* verifica que coincida con el tipo de retorno declarado *)
                  val tyresult = tipoRetorno nl result
                  val _ = if tiposIguales tybody tyresult then ()
                          else error ("cuerpo de la función \""^name^"\" incorrecto", nl)
                  (* crea el fragmento de la función *)
                  val _ = functionDec (expbody, lev, tybody=TUnit)
                  (* post *)
                  val _ = (postFunctionDec(); popLevel())
                in
                  ()
                end

              (* traduce todas las funciones del batch *)
              val _ = List.app trbody (ListPair.zip (fs, levels))
            in
              (venv', tenv, [])
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
