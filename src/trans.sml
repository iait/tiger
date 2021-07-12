structure trans :> trans = struct

  open frame
  open tree
  open temp
  open ast
  open stack

  exception Break
  exception DivByZero
  
  (* nivel de anidamiento estático *)
  type level = {parent: frame option, frame: frame, level: int}
  
  (* acceso a la variable *)
  type access = frame.access
  
  type frag = frame.frag

  val fragList = ref ([]: frag list)

  (* nivel de anidamiento actual y función para accederlo *)
  val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
  fun getActualLev() = !actualLevel

  (* level compartido por todas las funciones de la standar library
   * que también será el level padre de _tigermain
   *)
  val outermost: level = {
    parent = NONE,
    frame = newFrame {name="outermost", formals=[]},
    level = getActualLev()
  }

  (* crea un nuevo level a partir del level padre *)
  fun newLevel {parent={parent, frame, level}, name, formals} = {
    parent = SOME frame,
    frame = newFrame {name=name, formals=formals},
    level = level + 1
  }

  (* TODO por qué se llama parent??? no sería el level actual *)
  fun allocArg {parent, frame, level} b = frame.allocArg frame b
  fun allocLocal {parent, frame, level} b = frame.allocLocal frame b
  fun formals {parent, frame, level} = frame.formals frame

  datatype exp = Ex of tree.exp
               | Nx of tree.stm
               | Cx of label * label -> tree.stm

  (* seq : tree.stm list -> tree.stm *)
  fun seq [] = EXP (CONST 0)
    | seq [s] = s
    | seq (x::xs) = SEQ (x, seq xs)

  (* unEx : trans.exp -> tree.exp *)
  fun unEx (Ex e) = e
    | unEx (Nx s) = ESEQ (s, CONST 0)
    | unEx (Cx cf) =
        let
          val r = newTemp()
          val t = newLabel()
          val f = newLabel()
        in
          ESEQ (
            seq [
              MOVE (TEMP r, CONST 1),
              cf (t, f),
              LABEL f,
              MOVE (TEMP r, CONST 0),
              LABEL t],
            TEMP r)
        end

  (* unNx : trans.exp -> tree.stm *)
  fun unNx (Ex e) = EXP e
    | unNx (Nx s) = s
    | unNx (Cx cf) =
        let
          val l = newLabel()
        in
          seq [cf (l,l), LABEL l]
        end

  (* unCx : trans.exp -> (label * label -> tree.stm) *)
  fun unCx (Nx s) = raise Fail ("Error: unCx (Nx...)")
    | unCx (Cx cf) = cf
    | unCx (Ex (CONST 0)) =
        (fn (t,f) => JUMP (NAME f, [f]))
    | unCx (Ex (CONST _)) =
        (fn (t,f) => JUMP (NAME t, [t]))
    | unCx (Ex e) =
        (fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

  (* datos globales: lista de fragmentos *)
  val datosGlobs = ref ([]: frag list)

(************************************************)
(********** Intermediate representation *********)
(************************************************)
  (* Ir : frag list -> string *)
  fun Ir e =
    let
      (* aux : trans.exp -> string *)
      fun aux (Ex e) = treepp.tree (EXP e)
        | aux (Nx s) = treepp.tree (s)
        | aux _ = raise Fail "bueno, a completar!"

      (* aux2 : frag -> string *)
      fun aux2 (PROC {body, frame}) = aux (Nx body)
        | aux2 (STRING (l, "")) = l^":\n"
        | aux2 (STRING ("", s)) = "\t"^s^"\n"
        | aux2 (STRING (l, s)) = l^":\t"^s^"\n"

      (* aux3 : frag list -> string *)
      fun aux3 [] = ""
        | aux3 (h::t) = (aux2 h)^(aux3 t)

    in 
      aux3 e
    end

  fun nombreFrame frame = print(".globl " ^ frame.name frame ^ "\n")

(***********************************************)
(********** Traducción de expresiones **********)
(***********************************************)

(*************** variables ***************)

 (* simpleVar : trans.access * int -> trans.exp
   * acc: cómo acceder a la variable desde su fp
   * lev: nivel de declaración de la variable
   *)
  fun simpleVar (acc, lev) =
    let
      (* distancia entre el nivel de declaración y uso *)
      val dist = getActualLev() - lev
      (* genera expr para acceder al fp de n niveles menos *)
      fun aux 0 = TEMP fp
        | aux n = MEM (BINOP (PLUS, aux (n-1), CONST fpPrevLev))
    in
      Ex (frame.exp acc (aux dist))
    end
  
  (* fieldVar : trans.exp * int -> trans.exp
   * var: expresión del record
   * field: índice del field a acceder
   *)
  fun fieldVar (var, field) = 
    let
      val r = unEx var
      val rr = newTemp()
      val checkNil = externalCall ("_checkNil", [TEMP rr])
      val exp = case field of
        0 => MEM (TEMP rr)
        | n => MEM (BINOP (PLUS, TEMP rr, CONST (n * frame.wSz)))
    in
      Ex (ESEQ (
        seq[
          MOVE (TEMP rr, r),
          EXP checkNil],
        exp))
    end
  
  (* subscriptVar : trans.exp * trans.exp -> trans.exp *)
  fun subscriptVar (arr, ind) =
    let
      val a = unEx arr
      val i = unEx ind
      val ra = newTemp()
      val ri = newTemp()
      val checkIndex = externalCall ("_checkIndex", [TEMP ra, TEMP ri])
      val i' = case i of
          CONST k => CONST (k*frame.wSz)
          | _ => BINOP (LSHIFT, TEMP ri, CONST frame.log2wSz)
    in
      Ex (ESEQ (
        seq[
          MOVE (TEMP ra, a),
          MOVE (TEMP ri, i),
          EXP checkIndex],
        MEM (BINOP (PLUS, TEMP ra, i'))))
    end

(*************** constantes ***************)

  fun unitExp() = Ex (CONST 0)

  fun nilExp() = Ex (CONST 0)

  fun intExp i = Ex (CONST i)

  fun stringExp (s: string) =
    let
      val l = newLabel()
      val newFrag : frag = newStringFrag l s
      val _ = datosGlobs := (newFrag::(!datosGlobs))
    in
      Ex (NAME l)
    end

(*************** llamada a función ***************)

  (* val callExp : 
      {name: temp.label, extern: bool, proc: bool, lev: level, args: trans.exp list}
      -> trans.exp *)
  fun callExp {name, extern, proc, lev: level, args} = 
    let
      (* función para calcular el static link *)
      fun staticLink callerLev calleeLev =
        let
          fun aux 0 = MEM (BINOP (PLUS, TEMP frame.fp, CONST fpPrevLev))
            | aux n = MEM (BINOP (PLUS, aux (n-1), CONST fpPrevLev))
        in
          if callerLev = calleeLev - 1 then
            TEMP frame.fp
          else if callerLev >= calleeLev then
            aux (callerLev - calleeLev)
          else
            raise Fail "no debería pasar: callerLev < calleeLev - 1"
        end

      (* función para preparar los argumentos como CONST, NAME o TEMP *)
      fun prepArgs [] stms exps = (stms, exps)
        | prepArgs (hd::tl) stms exps = case unEx hd of
            CONST c => prepArgs tl stms ((CONST c)::exps)
            | NAME l => prepArgs tl stms ((NAME l)::exps)
            | TEMP r => prepArgs tl stms ((TEMP r)::exps)
            | e => let 
                     val temp = newTemp() 
                   in 
                     prepArgs tl (MOVE (TEMP temp, e)::stms) (TEMP temp:: exps)
                   end

      (* sentencias y expresiones de argumentos preparados *)
      val (argStms, argExps) = prepArgs args [] []

      (* agrega el static link si la función NO es externa *)
      val argExps' = 
        if extern then argExps
        else (staticLink (getActualLev()) (#level(lev)))::argExps

      (* crea un temporal para el retorno si la función devuelve algo *)
      val returnTemp = if proc then NONE else SOME (newTemp())
    in
      if proc then 
        Nx (seq (argStms @ [
          EXP (CALL (NAME name, argExps'))]))
      else 
        Ex (ESEQ (
          seq (argStms @ [
            MOVE (TEMP (valOf returnTemp), CALL (NAME name, argExps'))]), 
          TEMP (valOf returnTemp)))
    end

(*************** operaciones binarias ***************)

  (* binOpIntExp : {left:trans.exp, oper:ast.oper, right:trans.exp} -> trans.exp *)
  fun binOpIntExp {left, oper, right} = 
    let
      val binop = case oper of
        PlusOp => PLUS
        | MinusOp => MINUS
        | TimesOp => MUL
        | DivideOp => DIV
        | _ => raise Fail "Invocación binOpIntExp con operador inválido"
    in
      Ex (BINOP (binop, unEx left, unEx right))
    end

  (* binOpIntRelExp: {left:trans.exp, oper:ast.oper, right:trans.exp} -> trans.exp *)
  fun binOpIntRelExp {left,oper,right} =
    let
      val relop = case oper of
        EqOp => EQ
        | NeqOp => NE
        | LtOp => LT
        | LeOp => LE
        | GtOp => GT
        | GeOp => GE
        | _ => raise Fail "Invocación binOpIntRelExp con operador inválido"
    in
      Cx (fn (t, f) => CJUMP (relop, unEx left, unEx right, t, f))
    end

  (* binOpStrExp : {left:trans.exp, oper:ast.oper, right:trans.exp} -> trans.exp *)
  fun binOpStrExp {left,oper,right} =
    let
      val relop = case oper of
        EqOp => EQ
        | NeqOp => NE
        | LtOp => LT
        | LeOp => LE
        | GtOp => GT
        | GeOp => GE
        | _ => raise Fail "Invocación binOpStrExp con operador inválido"
      val stringCompare = externalCall ("_stringCompare", [unEx left, unEx right])
    in
      Cx (fn (t, f) => CJUMP (relop, stringCompare, CONST 0, t, f))
    end

    (* nilCompare : {record:trans.exp, oper:ast.oper} -> trans.exp *)
    fun nilCompare {record, oper} =
      let
        val relop = case oper of
          EqOp => EQ
          | NeqOp => NE
          | _ => raise Fail "Invocación nilCompare con operador inválido"
        val nilCompare = externalCall ("_nilCompare", [unEx record])
      in
        Cx (fn (t, f) => CJUMP (relop, nilCompare, CONST 1, t, f))
      end

(*************** inicialización de record y array ***************)

  (* recordExp : (trans.exp * int) list -> trans.exp *)
  fun recordExp l =
    let
      val recAddr = newTemp()
      fun initRecord [] = []
        | initRecord ((e, i)::tl) = 
            (MOVE (BINOP (PLUS, TEMP recAddr, CONST (i*frame.wSz)), unEx e))::(initRecord tl)
      val initStms = initRecord l
      val allocRecord = externalCall ("_allocRecord", [CONST (List.length l)])
    in
      Ex (ESEQ (
        seq (MOVE (TEMP recAddr, allocRecord)::initStms),
        TEMP recAddr))
    end

  (* arrayExp : {size: trans.exp, init: trans.exp} -> trans.exp *)
  fun arrayExp {size, init} =
    let
      val sizeExp = unEx size
      val initExp = unEx init
      val arrayAddr = newTemp()
      val allocArray = externalCall ("_allocArray", [sizeExp, initExp])
    in
      Ex (ESEQ (
        MOVE (TEMP arrayAddr, allocArray),
        TEMP arrayAddr))
    end

(*************** condicionales if ***************)

  (* ifThenExp : {test: trans.exp, then': trans.exp} -> trans.exp *)
  fun ifThenExp {test, then'} =
    let
      val thenLabel = newLabel()
      val joinLabel = newLabel()
      val condFunc = unCx test
      val thenStm = unNx then'
    in
      Nx (seq [
          condFunc (thenLabel, joinLabel),
          LABEL thenLabel,
          thenStm,
          LABEL joinLabel])
    end

  (* ifThenElseExp : {test: trans.exp, then': trans.exp, else': trans.exp} -> trans.exp *)
  fun ifThenElseExp {test, then', else'} =
    let
      val thenLabel = newLabel()
      val elseLabel = newLabel()
      val joinLabel = newLabel()
      val returnTemp = newTemp()
      val condFunc = unCx test
      val thenExp = unEx then'
      val elseExp = unEx else'
    in
      Ex (ESEQ (
        seq [
          condFunc (thenLabel, elseLabel),
          LABEL thenLabel,
          MOVE (TEMP returnTemp, thenExp),
          JUMP (NAME joinLabel, [joinLabel]),
          LABEL elseLabel,
          MOVE (TEMP returnTemp, elseExp),
          LABEL joinLabel],
        TEMP returnTemp))
    end

  (* ifThenElseExpUnit : {test: trans.exp, then': trans.exp, else': trans.exp} -> trans.exp *)
  fun ifThenElseExpUnit {test, then', else'} =
    let
      val thenLabel = newLabel()
      val elseLabel = newLabel()
      val joinLabel = newLabel()
      val condFunc = unCx test
      val thenStm = unNx then'
      val elseStm = unNx else'
    in
      Nx (seq [
          condFunc (thenLabel, elseLabel),
          LABEL thenLabel,
          thenStm,
          JUMP (NAME joinLabel, [joinLabel]),
          LABEL elseLabel,
          elseStm,
          LABEL joinLabel])
    end

(*************** ciclos while y for ***************)

  (* while y for necesitan la última etiqueta para un break *)
  local
    val salidas: label option Pila = nuevaPila1 NONE
  in
    val pushSalida = pushPila salidas
    fun popSalida() = popPila salidas
    fun topSalida() = case topPila salidas of
          SOME l => l
          | NONE => raise Break
  end

  fun preLoopExp() = pushSalida (SOME (newLabel()))
  
  fun breakExp() = 
    let
      val doneLabel = topSalida()
    in
      Nx (JUMP (NAME doneLabel, [doneLabel]))
    end
  
  fun postLoopExp() = popSalida()
  
  (* whileExp : {test: trans.exp, body: trans.exp} -> trans.exp *)
  fun whileExp {test: exp, body: exp} =
    let
      val condFunc = unCx test
      val bodyExp = unNx body
      val testLabel = newLabel()
      val bodyLabel = newLabel()
      val doneLabel = topSalida()
    in
      Nx (seq[
        LABEL testLabel,
        condFunc (bodyLabel, doneLabel),
        LABEL bodyLabel,
        bodyExp,
        JUMP (NAME testLabel, [testLabel]),
        LABEL doneLabel])
    end
  
  (* forExp : {lo: trans.exp, hi: trans.exp, var: trans.exp, body: trans.exp} -> trans.exp *)
  fun forExp {lo, hi, var, body} =
    let
      val hiTemp = newTemp()
      val loExp = unEx lo
      val hiExp = unEx hi
      val varExp = unEx var
      val bodyExp = unNx body
      val incLabel = newLabel()
      val bodyLabel = newLabel()
      val doneLabel = topSalida()
    in
      Nx (seq [
        MOVE (TEMP hiTemp, hiExp),
        MOVE (varExp, loExp),
        CJUMP (LE, varExp, TEMP hiTemp, bodyLabel, doneLabel),
        LABEL bodyLabel,
        bodyExp,
        CJUMP (LT, varExp, TEMP hiTemp, incLabel, doneLabel),
        LABEL incLabel,
        MOVE (varExp, BINOP (PLUS, varExp, CONST 1)),
        JUMP (NAME bodyLabel, [bodyLabel]),
        LABEL doneLabel])
    end

(*************** declaraciones let ***************)

  (* letExp : trans.exp list * trans.exp -> trans.exp *)
  fun letExp ([], body) = Ex (unEx body)
    | letExp (decs, body) = Ex (ESEQ (seq (map unNx decs), unEx body))

(*************** secuencia ***************)
  
  (* seqExp : trans.exp list -> trans.exp *)
  fun seqExp ([]: exp list) = Nx (EXP (CONST 0))
    | seqExp (exps: exp list) =
      let
        fun unx [] = raise Fail "no debería recibir lista vacía"
          | unx [e] = []
          | unx (s::ss) = (unNx s)::(unx ss)
      in
        case List.last exps of
          Nx s =>
            let val unexps = map unNx exps
            in Nx (seq unexps) end
          | Ex e => Ex (ESEQ (seq (unx exps), e))
          | cond => Ex (ESEQ (seq (unx exps), unEx cond))
      end

(*************** asignación ***************)

  (* assignExp : {var: trans.exp, exp: trans.exp} -> trans.exp *)
  fun assignExp {var, exp} =
    let
      val v = unEx var
      val vl = unEx exp
    in
      Nx (MOVE (v, vl))
    end

(*************** declaración de variable ***************)

  (* varDec : trans.access -> trans.exp *)
  fun varDec acc = simpleVar (acc, getActualLev())

(*************** declaración de función ***************)

  (* procEntryExit : {level: trans.level, body: trans.exp} -> unit *)
  fun procEntryExit {level: level, body: exp} =
    let
      val frame = #frame(level)
      val label = STRING (name frame, "")
      val proc = PROC {frame = frame, body = unNx body}
      val final = STRING ("#############", "")
    in  
      datosGlobs := (!datosGlobs @ [label, proc, final])
    end

  fun getResult() = !datosGlobs

  (* preFunctionDec : unit -> unit *)
  fun preFunctionDec() = (
    pushSalida NONE;
    actualLevel := !actualLevel+1)

  (* functionDec : trans.exp * trans.level * bool -> unit *)
  fun functionDec (exp, lev, proc) =
    let
      val frame = #frame(lev)
      val body =
        if proc then unNx exp
        else MOVE (TEMP rv, unEx exp)
      val body' = procEntryExit1 (frame, body)
    in
      procEntryExit {level = lev, body = Nx body'}
    end

  (* postFunctionDec : unit -> unit *)
  fun postFunctionDec() = (
    popSalida();
    actualLevel := !actualLevel-1)

end
