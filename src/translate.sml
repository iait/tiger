structure translate :> translate = struct

  open frame
  open tree
  open temp
  open ast
  open stack

  exception Break
  exception DivByZero

  type level = {parent: frame option, frame: frame, level: int}
  type access = frame.access
  val TODO : access = todo
  type frag = frame.frag

  val fragList = ref ([]: frag list)

  val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
  fun getActualLev() = !actualLevel

  val outermost: level = {
    parent = NONE,
    frame = newFrame {name="_tigermain", formals=[]},
    level = getActualLev()
  }

  fun newLevel {parent={parent, frame, level}, name, formals} = {
    parent = SOME frame,
    frame = newFrame {name=name, formals=formals},
    level = level + 1}

  fun allocArg {parent, frame, level} b = frame.allocArg frame b

  fun allocLocal {parent, frame, level} b = frame.allocLocal frame b

  fun formals {parent, frame, level} = frame.formals frame

  datatype exp = Ex of tree.exp
               | Nx of tree.stm
               | Cx of label * label -> tree.stm
               | scaf
  val SCAF = scaf

  fun seq [] = EXP (CONST 0)
    | seq [s] = s
    | seq (x::xs) = SEQ (x, seq xs)

  fun unEx (Ex e) = e
    | unEx (Nx s) = ESEQ(s, CONST 0)
    | unEx (Cx cf) =
    let
      val r = newTemp()
      val t = newLabel()
      val f = newLabel()
    in
      ESEQ(seq [MOVE(TEMP r, CONST 1),
        cf (t, f),
        LABEL f,
        MOVE(TEMP r, CONST 0),
        LABEL t],
        TEMP r)
    end
    | unEx scaf = raise Fail "SCAF!"

  fun unNx (Ex e) = EXP e
    | unNx (Nx s) = s
    | unNx (Cx cf) =
    let
      val t = newLabel()
      val f = newLabel()
    in
      seq [cf(t,f),
        LABEL t,
        LABEL f]
    end
    | unNx scaf = raise Fail "SCAF!"

  fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
    | unCx (Cx cf) = cf
    | unCx (Ex (CONST 0)) =
    (fn (t,f) => JUMP(NAME f, [f]))
    | unCx (Ex (CONST _)) =
    (fn (t,f) => JUMP(NAME t, [t]))
    | unCx (Ex e) =
    (fn (t,f) => CJUMP(NE, e, CONST 0, t, f))
    | unCx scaf = raise Fail "SCAF!"

  fun Ir(e) =
    let fun aux(Ex e) = it.tree(EXP e)
      | aux(Nx s) = it.tree(s)
      | aux _ = raise Fail "bueno, a completar!"
      fun aux2(PROC{body, frame}) = aux(Nx body)
      | aux2(STRING(l, "")) = l^":\n"
      | aux2(STRING("", s)) = "\t"^s^"\n"
      | aux2(STRING(l, s)) = l^":\t"^s^"\n"
      fun aux3 [] = ""
      | aux3(h::t) = (aux2 h)^(aux3 t)
    in  aux3 e end
  fun nombreFrame frame = print(".globl " ^ frame.name frame ^ "\n")

  (* While y for necesitan la última etiqueta para un break *)
  local
    val salidas: label option Pila = nuevaPila1 NONE
  in
    val pushSalida = pushPila salidas
    fun popSalida() = popPila salidas
    fun topSalida() = case topPila salidas of
          SOME l => l
          | NONE => raise Fail "break incorrecto!"
  end

  val datosGlobs = ref ([]: frag list)
  fun procEntryExit{level: level, body} =
    let val label = STRING(name(#frame level), "")
      val body' = PROC{frame= #frame level, body=unNx body}
      val final = STRING(";;-------", "")
    in  datosGlobs:=(!datosGlobs@[label, body', final]) end
  fun getResult() = !datosGlobs
  
  fun stringLen s =
    let
      fun aux [] = 0
        | aux (t1::t2::t3::t4::t) = 
            if t1=(#"\\") andalso t2=(#"x") then 1 + (aux t)
            else 1 + (aux (t2::t3::t4::t))
        | aux (_::t) = 1 + (aux t)
    in
      aux (explode s)
    end
  
  fun stringExp(s: string) =
    let val l = newLabel()
      val len = ".long "^makestring(stringLen s)
      val str = ".string \""^s^"\""
      val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING("", str)])
    in  Ex(NAME l) end
  fun preFunctionDec() =
    (pushSalida(NONE);
    actualLevel := !actualLevel+1)
  fun functionDec(e, l, proc) =
    let val body =
          if proc then unNx e
          else MOVE(TEMP rv, unEx e)
      val body' = procEntryExit1(#frame l, body)
      val () = procEntryExit{body=Nx body', level=l}
    in  Ex(CONST 0) end
  fun postFunctionDec() =
    (popSalida(); actualLevel := !actualLevel-1)
  
  fun unitExp() = Ex (CONST 0)
  
  fun nilExp() = Ex (CONST 0)
  
  fun intExp i = Ex (CONST i)
  
  fun simpleVar(acc, nivel) =
    SCAF (*COMPLETAR*)
  
  fun varDec(acc) = simpleVar(acc, getActualLev())
  
  fun fieldVar(var, field) = 
    SCAF (*COMPLETAR*)
  
  fun subscriptVar(arr, ind) =
  let
    val a = unEx arr
    val i = unEx ind
    val ra = newTemp()
    val ri = newTemp()
  in
    Ex( ESEQ(seq[MOVE(TEMP ra, a),
      MOVE(TEMP ri, i),
      EXP(externalCall("_checkindex", [TEMP ra, TEMP ri]))],
      MEM(BINOP(PLUS, TEMP ra,
        BINOP(MUL, TEMP ri, CONST frame.wSz)))))
  end
  
  fun recordExp l =
    SCAF (*COMPLETAR*)
  
  fun arrayExp{size, init} =
  let
    val s = unEx size
    val i = unEx init
  in
    Ex (externalCall("allocArray", [s, i]))
  end
  
  fun callExp (name,external,isproc,lev:level,ls) = 
    Ex (CONST 0) (*COMPLETAR*)
  
  fun letExp ([], body) = Ex (unEx body)
   |  letExp (inits, body) = Ex (ESEQ(seq inits,unEx body))
  
  fun breakExp() = 
    SCAF (*COMPLETAR*)
  
  fun seqExp ([]:exp list) = Nx (EXP(CONST 0))
    | seqExp (exps:exp list) =
      let
        fun unx [e] = []
          | unx (s::ss) = (unNx s)::(unx ss)
          | unx[] = []
      in
        case List.last exps of
          Nx s =>
            let val unexps = map unNx exps
            in Nx (seq unexps) end
          | Ex e => Ex (ESEQ(seq(unx exps), e))
          | cond => Ex (ESEQ(seq(unx exps), unEx cond))
      end
  
  fun preWhileForExp() = pushSalida(SOME(newLabel()))
  
  fun postWhileForExp() = (popSalida(); ())
  
  fun whileExp {test: exp, body: exp, lev:level} =
  let
    val cf = unCx test
    val expb = unNx body
    val (l1, l2, l3) = (newLabel(), newLabel(), topSalida())
  in
    Nx (seq[LABEL l1,
      cf(l2,l3),
      LABEL l2,
      expb,
      JUMP(NAME l1, [l1]),
      LABEL l3])
  end
  
  fun forExp {lo, hi, var, body} =
    SCAF (*COMPLETAR*)
  
  fun ifThenExp{test, then'} =
    SCAF (*COMPLETAR*)
  
  fun ifThenElseExp {test,then',else'} =
    SCAF (*COMPLETAR*)
  
  fun ifThenElseExpUnit {test,then',else'} =
    SCAF (*COMPLETAR*)
  
  fun assignExp{var, exp} =
  let
    val v = unEx var
    val vl = unEx exp
  in
    Nx (MOVE(v,vl))
  end
  
  fun binOpIntExp {left, oper, right} = 
    SCAF (*COMPLETAR*)
  
  fun binOpIntRelExp {left,oper,right} =
    SCAF (*COMPLETAR*)
  
  fun binOpStrExp {left,oper,right} =
    SCAF (*COMPLETAR*)

end
