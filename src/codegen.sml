structure codegen :> codegen = struct

  open temp
  open tree
  open assem
  open frame
  open regalloc
  open graph
  open flow
  open TextIO
  open util

  (* lista de instrucciones *)
  val ilist = ref ([] : instr list)

  (* agrega una instrucción a la lista *)
  fun emit i = ilist := (i :: !ilist)

  fun emitOper assem src dst = 
    emit (OPER {assem=assem, src=src, dst=dst, jmp=[]})

  fun emitJump assem jmp = 
    emit (OPER {assem=assem, src=[], dst=[], jmp=jmp})

  (* formatea el entero para el assembler *)
  fun fmt n = if n < 0 then "-"^(Int.toString (~n)) else (Int.toString n)

(********************** munchExp : tree.exp -> temp **********************)
    (* CONST n *)
  fun munchExp (CONST n) =
        let
          val t = newTemp()
          val _ = munchStm (MOVE (TEMP t, CONST n))
        in t end

    (* NAME l *)
    | munchExp (NAME l) =
        let
          val t = newTemp()
          val _ = munchStm (MOVE (TEMP t, NAME l))
        in t end

    (* TEMP t *)
    | munchExp (TEMP t) = t

    (* BINOP... *)
    | munchExp (BINOP (DIV, e1, e2)) =
        let
          val _ = munchStm (MOVE (TEMP rax, e1))
          val _ = emitOper "cqo" [rax] [rdx]
          val _ = case e2 of
              MEM (BINOP (PLUS, CONST n, e)) =>
                emitOper ("idivq "^(fmt n)^"(`s0)") [munchExp e, rax, rdx] [rax, rdx]
            | MEM (BINOP (PLUS, e, CONST n)) =>
                emitOper ("idivq "^(fmt n)^"(`s0)") [munchExp e, rax, rdx] [rax, rdx]
            | MEM e => 
                emitOper "idivq (`s0)" [munchExp e, rax, rdx] [rax, rdx]
            | _ =>
                emitOper "idivq `s0" [munchExp e2, rax, rdx] [rax, rdx]
        in rax end
    
    | munchExp (BINOP (oper, e1, e2)) =
        let
          val instr = case oper of
              PLUS => "addq"
            | MINUS => "subq"
            | MUL => "imulq"
            | _ => raise Fail "operación no soportada"
          val t = newTemp()
          val _ = munchStm (MOVE (TEMP t, e1))
          val _ = case e2 of
              CONST n =>
                emitOper (instr^" $"^(fmt n)^", `d0") [t] [t]
            | MEM (BINOP (PLUS, CONST n, e)) =>
                emitOper (instr^" "^(fmt n)^"(`s0), `d0") [munchExp e, t] [t]
            | MEM (BINOP (PLUS, e, CONST n)) =>
                emitOper (instr^" "^(fmt n)^"(`s0), `d0") [munchExp e, t] [t]
            | MEM e => 
                emitOper (instr^" (`s0), `d0") [munchExp e, t] [t]
            | _ =>
                emitOper (instr^" `s0, `d0") [munchExp e2, t] [t]
        in t end

    (* MEM *)
    | munchExp (MEM e) =
        let
          val t = newTemp()
          val _ = munchStm (MOVE (TEMP t, MEM e))
        in t end

    | munchExp _ = raise Fail "no debería llegar este tipo de expresión"

(************** munchArgs : tree.exp list -> temp list -> temp list ****************)
  (* genera instrucciones para mover cada argumento a la posición correcta *)
  and munchArgs [] _ = []
    | munchArgs es [] = (List.app munchStack es; [])
    | munchArgs (e::es) (r::rs) =
        (munchStm (MOVE (TEMP r, e)); r::(munchArgs es rs))

(************************** munchStack : tree.exp -> unit **************************)
  (* pone el valor de la expresión en la pila *)
  and munchStack (CONST n) =
        emitOper ("pushq $"^(fmt n)) [] []
    | munchStack (NAME l) =
        emitOper ("pushq $"^l) [] []
    | munchStack (TEMP t) =
        emitOper "pushq `s0" [t] []
    | munchStack (MEM (TEMP t)) =
        emitOper "pushq (`s0)" [t] []
    | munchStack (MEM (BINOP (PLUS, CONST n, e))) =
        emitOper ("pushq "^(fmt n)^"(`s0)") [munchExp e] []
    | munchStack (MEM (BINOP (PLUS, e, CONST n))) =
        munchStack (MEM (BINOP (PLUS, CONST n, e)))
    | munchStack (MEM e) =
        emitOper "pushq (`s0)" [munchExp e] []
    | munchStack e =
        emitOper "pushq `s0" [munchExp e] []

(*************************** munchStm : tree.stm -> unit ***************************)
    (* invocación a función *)
    (* MOVE (TEMP t, CALL... *)
  and munchStm (MOVE (TEMP t, CALL (NAME f, args))) =
        let
          (* para mantener alineamiento de 16-bytes en el stack *)
          val stackRegs = length args - length frame.argRegs
          val _ = 
            if stackRegs < 0 orelse stackRegs mod 2 = 0 then ()
            else emitOper "pushq $0" [] []
          val src = munchArgs args frame.argRegs
        in
          emitOper ("call "^f) src frame.callerSave;
          if t=rv then () else emit (MOV {assem="movq `s0, `d0", dst=t, src=rv})
        end
    | munchStm (MOVE (TEMP t, CALL _)) =
        raise Fail "no debería existir este tipo de call a función"

    (* invocación a procedimiento *)
    (* EXP (CALL... *)
    | munchStm (EXP (CALL (NAME f, args))) =
        let
          (* para mantener alineamiento de 16-bytes en el stack *)
          val stackRegs = length args - length frame.argRegs
          val _ = 
            if stackRegs < 0 orelse stackRegs mod 2 = 0 then ()
            else emitOper "pushq $0" [] []
          val src = munchArgs args frame.argRegs
        in
          emitOper ("call "^f) src frame.callerSave
        end
    | munchStm (EXP _) =
        raise Fail "no debería existir otro tipo de EXP"

    (* MOVE (TEMP... *)
    | munchStm (MOVE (TEMP t, e)) = (case e of
        MEM (BINOP (PLUS, CONST n, e')) =>
          emitOper ("movq "^(fmt n)^"(`s0),`d0") [munchExp e'] [t]
      | MEM (BINOP (PLUS, e', CONST n)) =>
          munchStm (MOVE (TEMP t, (BINOP (PLUS, CONST n, e'))))
      | MEM e' =>
          emitOper ("movq (`s0), `d0") [munchExp e'] [t]
      | NAME l =>
          emitOper ("movq $"^l^", `d0") [] [t]
      | CONST n =>
          emitOper ("movq $"^(fmt n)^", `d0") [] [t]
      | TEMP u =>
          if t=u then ()
          else emit (MOV {assem="movq `s0, `d0", src=u, dst=t})
      | _ =>
          emit (MOV {assem="movq `s0, `d0", src=munchExp e, dst=t}))
    
    (* MOVE (MEM... *)
    | munchStm (MOVE (MEM e1, e2)) = (case (e1, e2) of
        (BINOP (PLUS, CONST n, e), NAME l) =>
          emitOper ("movq $"^l^", "^(fmt n)^"(`s0)") [munchExp e] []
      | (BINOP (PLUS, CONST n, e), CONST m) =>
          emitOper ("movq $"^(fmt m)^", "^(fmt n)^"(`s0)") [munchExp e] []
      | (BINOP (PLUS, CONST n, e), _) =>
          emitOper ("movq `s0, "^(fmt n)^"(`s1)") [munchExp e2, munchExp e] []
      | (BINOP (PLUS, e, CONST n), _) =>
          emitOper ("movq `s0, "^(fmt n)^"(`s1)") [munchExp e2, munchExp e] []
      | (_, NAME l) =>
          emitOper ("movq $"^l^", (`s0)") [munchExp e1] []
      | (_, CONST n) =>
          emitOper ("movq $"^(fmt n)^", (`s0)") [munchExp e1] []
      | _ =>
          emitOper ("movq `s0, (`s1)") [munchExp e2, munchExp e1] [])

    | munchStm (MOVE _) = raise Fail "no debería haber otro tipo de MOVE"

    (* JUMP... *)
    | munchStm (JUMP (NAME l, ls)) =
        emitJump ("jmp "^l) ls
    | munchStm (JUMP _) = raise Fail "no debería haber otro tipo de JUMP"

    (* CJUMP... *)
    | munchStm (CJUMP (reloper, e1, e2, l1, l2)) =
        let
          (* emitComp : tree.relop -> tree.exp -> tree.exp -> tree.relop *)
          fun emitComp oper (MEM (BINOP (PLUS, CONST n1, e1))) (CONST n2) =
                (emitOper ("cmpq $"^(fmt n2)^", "^(fmt n1)^"(`s0)") [munchExp e1] []; oper)
            | emitComp oper (MEM (BINOP (PLUS, CONST n1, e1))) e2 =
                (emitOper ("cmpq `s0, "^(fmt n1)^"(`s1)") [munchExp e2, munchExp e1] []; oper)
            | emitComp oper (MEM (BINOP (PLUS, e1, CONST n1))) e2 =
                emitComp oper (MEM (BINOP (PLUS, CONST n1, e1))) e2
            | emitComp oper (MEM e1) (CONST n2) =
                (emitOper ("cmpq $"^(fmt n2)^", (`s0)") [munchExp e1] []; oper)
            | emitComp oper (MEM e1) e2 =
                (emitOper ("cmpq `s0, (`s1)") [munchExp e2, munchExp e1] []; oper)
            | emitComp oper e1 (CONST n2) =
                (emitOper ("cmpq $"^(fmt n2)^", `s0") [munchExp e1] []; oper)
            | emitComp oper e1 (MEM e2) =
                emitComp (notRel oper) (MEM e2) e1
            | emitComp oper e1 e2 =
                (emitOper ("cmpq `s0, `s1") [munchExp e2, munchExp e1] []; oper)
          (* emite la instrucción de comparación y devuelve la operación correspondiente *)
          val reloper' = emitComp reloper e1 e2
        in
          case reloper' of
              EQ => emitJump ("je "^l1) [l1, l2]
            | NE => emitJump ("jne "^l1) [l1, l2]
            | LT => emitJump ("jl "^l1) [l1, l2]
            | GT => emitJump ("jg "^l1) [l1, l2]
            | LE => emitJump ("jle "^l1) [l1, l2]
            | GE => emitJump ("jge "^l1) [l1, l2]
            | _ => raise Fail "operación no soportada"
        end

    (* LABEL l *)
    | munchStm (LABEL l) = emit (LAB {assem=l^":", lab=l})

    | munchStm _ = raise Fail "no debería llegar este tipo de sentencias"

  (* originalCodegen : frame.frame -> tree.stm -> assem.instr list *)
  fun originalCodegen frame stm = (munchStm stm; rev (!ilist))

  (* Genera el código assembler para un procedimiento y lo vuelca en el outstream *)
  (* codegen : outstream -> bool -> (stm list * frame) -> unit *)
  fun codegen out debug (stms, frame) =
    let
      val _ = ilist := []
      val _ = List.app munchStm stms
      val instrs = rev (!ilist)
      (* imprime las instrucciones antes de la asignación de registros *)
      val _ =
        if debug then
          (print "Instrucciones originales\n";
          print ((name frame)^":\n");
          List.app (print o (format id)) instrs)
        else ()
      (* calcula la asignación de registros *)
      val (is, saytemp) = regalloc debug frame (procEntryExit2 (frame, instrs))
      (* genera prólogo y epílogo de la función *)
      val {prolog, body, epilog} = procEntryExit3 (frame, is)
      (* escribe en el archivo assembler *)
      fun writeOut s = output (out, s)
    in
      writeOut prolog;
      List.app (writeOut o (format saytemp)) body;
      writeOut epilog
    end 

end