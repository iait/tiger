structure codegen :> codegen = struct

  open temp
  open tree
  open assem
  open frame

  (* codegen : frame.frame -> tree.stm -> assem.instr list *)
  fun codegen frame stm =
    let
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

      (* result : (temp -> ) -> temp *)
      fun result gen =
        let
          val t = newTemp()
        in
          gen t; t
        end

      (* munchExp : tree.exp -> temp *)
      fun munchExp exp = newTemp()
      
      (* munchArgs : tree.exp list -> temp list -> temp list *)
      (* genera instrucciones para mover cada argumento a la posición correcta *)
      and munchArgs [] _ = []
        | munchArgs es [] = (List.app munchStack es; [])
        | munchArgs (e::es) (r::rs) = 
            (munchStm (MOVE (TEMP r, e)); r::(munchArgs es rs))

      (* munchStack : tree.exp -> unit *)
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

      (* munchStm : tree.stm -> unit *)
        (* invocación a función *)
      and munchStm (MOVE (TEMP t, CALL (f, args))) =
            emit (OPER {assem="call `s0", src=(munchExp f)::(munchArgs args frame.argRegs), dst=frame.callerSave, jmp=[]})
            (* para mantener alineamiento de 16-bytes en el stack *)

        (* invocación a procedimiento *)
        (* EXP (CALL... *)
        | munchStm (EXP (CALL (NAME f, args))) = ()

        (* MOVE (TEMP... *)
        | munchStm (MOVE (TEMP t, e)) = (case e of
            MEM (BINOP (PLUS, CONST n, e')) =>
              emitOper ("movq "^(fmt n)^"(`s0),`d0") [munchExp e'] [t]
          | MEM (BINOP (PLUS, e', CONST n)) =>
              munchStm (MOVE (TEMP t, (BINOP (PLUS, CONST n, e'))))
          | MEM e' =>
              emitOper ("movq (`s0),`d0") [munchExp e'] [t]
          | NAME l =>
              emitOper ("movq $"^l^",`d0") [] [t]
          | CONST n =>
              emitOper ("movq $"^(fmt n)^",`d0") [] [t]
          | _ =>
              emit (MOVE {assem="movq `s0,`d0", src=munchExp e, dst=t}))
        
        (* MOVE (MEM... *)
        | munchStm (MOVE (MEM e1, e2)) = (case (e1, e2) of
            (BINOP (PLUS, CONST n, e), NAME l) =>
              emitOper ("movq $"^l^","^(fmt n)^"(`s0)") [munchExp e] []
          | (BINOP (PLUS, CONST n, e), CONST m) =>
              emitOper ("movq $"^(fmt m)^","^(fmt n)^"(`s0)") [munchExp e] []
          | (BINOP (PLUS, CONST n, e), _) =>
              emitOper ("movq `s0,"^(fmt n)^"(`s1)") [munchExp e2, munchExp e] []
          | (BINOP (PLUS, e, CONST n), _) =>
              munchStm (MOVE (BINOP (PLUS, CONST n, e)), e2)
          | (_, NAME l) =>
              emitOper ("movq $"^l^",(`s0)") [munchExp e1] []
          | (_, CONST n) =>
              emitOper ("movq $"^(fmt n)^",(`s0)") [munchExp e1] []
          | _ =>
              emitOper ("movq `s0,(`s1)") [munchExp e2, munchExp e1] [])

        | munchStm (MOVE _) = raise Fail "no debería haber otro tipo de move"



        | munchStm stm = ()
    in
      munchStm stm; rev (!ilist)
    end

end