structure seman :> seman = struct

  open ast
  open entry
  open translate
  open table

  type expty = {exp: exp, ty: Tipo}

  type venv = (string, EnvEntry) Tabla
  type tenv = (string, Tipo) Tabla

  val tabTipos : (string, Tipo) Tabla = 
        tabInserList (tabNueva(), [("int", TInt), ("string", TString)])

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
    | tiposIguales (TRecord (_, u1)) (TRecord (_, u2)) = (u1=u2)
    | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
    | tiposIguales a b = (a=b)

  (* transExp : venv * tenv -> ast.exp -> expty *)
  fun transExp (venv, tenv) =
    let 
      fun error (s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")

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
                else error ("tipos no comparables", nl)
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
                else error ("tipos no comparables", nl)
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
                  | _        => raise Fail "No debería pasar! (3)"
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
                | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
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
              val {exp=vexp,ty=vty} = trvar (var, nl)
              val {exp, ty} = trexp exp
              val _ = if tiposIguales vty ty then () else
                        error ("error de tipos en asignación", nl)
            in
              {exp=SCAF, ty=TUnit}
            end
(*TODO*)
        | trexp (IfExp({test, then', else'=SOME else'}, nl)) =
        let val {exp=testexp, ty=tytest} = trexp test
            val {exp=thenexp, ty=tythen} = trexp then'
            val {exp=elseexp, ty=tyelse} = trexp else'
        in
          if tytest=TInt andalso tiposIguales tythen tyelse then {exp=SCAF, ty=tythen}
          else error("Error de tipos en if" ,nl)
        end
      | trexp(IfExp({test, then', else'=NONE}, nl)) =
        let val {exp=exptest,ty=tytest} = trexp test
            val {exp=expthen,ty=tythen} = trexp then'
        in
          if tytest=TInt andalso tythen=TUnit then {exp=SCAF, ty=TUnit}
          else error("Error de tipos en if", nl)
        end
      | trexp(WhileExp({test, body}, nl)) =
        let
          val ttest = trexp test
          val tbody = trexp body
        in
          if (#ty ttest) = TInt andalso #ty tbody = TUnit then {exp=SCAF, ty=TUnit}
          else if (#ty ttest) <> TInt then error("Error de tipo en la condición", nl)
          else error("El cuerpo de un while no puede devolver un valor", nl)
        end
      | trexp(ForExp({var, escape, lo, hi, body}, nl)) =
        {exp=SCAF, ty=TUnit} (*COMPLETAR*)
      | trexp(LetExp({decs, body}, _)) =
        let
          val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
          val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
        in 
          {exp=SCAF, ty=tybody}
        end
      | trexp(BreakExp nl) =
        {exp=SCAF, ty=TUnit} (*COMPLETAR*)
      | trexp(ArrayExp({typ, size, init}, nl)) =
        {exp=SCAF, ty=TUnit} (*COMPLETAR*)

    (* traducción de variables *)
    and trvar (SimpleVar s, nl) =
          let
            val tipo = case tabBusca (s, venv) of
              NONE => error ("variable inexistente \""^s^"\"", nl)
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
            val tipo = case List.find (fn (sy,_,_) => s=sy) cs of
              NONE => error ("no existe el campo \""^s^"\" en el record", nl)
              | SOME (_,r,_) => !r
          in
            {exp=SCAF, ty=tipo}
          end
      | trvar (SubscriptVar (v, e), nl) =
          let
            (* traduce la variable v y la expresión e *)
            val (vexp, tipo) = case trvar (v, nl) of
              {exp, ty=TArray (r, u)} => (exp, !r)
              | _ => error ("se esperaba un tipo array", nl)
            val iexp = case trexp e of
              {exp, ty=TInt} => exp
              | _ => error ("el índice debe ser de tipo entero", nl)
          in
            {exp=SCAF, ty=tipo}
          end

      (* traducción de declaraciones *)
      and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
        (venv, tenv, []) (*COMPLETAR*)
      | trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
        (venv, tenv, []) (*COMPLETAR*)
      | trdec (venv,tenv) (FunctionDec fs) =
        (venv, tenv, []) (*COMPLETAR*)
      | trdec (venv,tenv) (TypeDec ts) =
        (venv, tenv, []) (*COMPLETAR*)
    in trexp end

  fun transProg ex =
        let
          val main = LetExp ({
                decs=[FunctionDec[({name="_tigermain", params=[], result=NONE, body=ex}, 0)]],
                body=UnitExp 0}, 0)
          val _ = transExp (tabVars, tabTipos) main
        in
          print "bien!\n"
        end

end
