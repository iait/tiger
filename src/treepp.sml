(* Tree Pretty Print *)
structure treepp :> treepp = struct

  open table
  open tree

  fun say s = s
  fun sayln s = s^"\n"

  fun indent 0 = ""
    | indent i = " "^indent(i-1)

  (* stm : tree.stm * int -> string *)
  fun stm (SEQ(a,b),d) =
        indent(d)^sayln("SEQ(")^stm(a,d+1)^
        sayln(",")^stm(b,d+1)^say(")")
    | stm (LABEL lab, d) = indent(d)^say("LABEL ")^say(lab)
    | stm (JUMP (e,_), d) =  indent(d)^say("JUMP(")^exp(e,d+1)^say(")")
    | stm (CJUMP (r,a,b,t,f),d) = 
        indent(d)^say("CJUMP(")^
        printRelOp(r)^sayln(",")^
        exp(a,d+1)^sayln(",")^exp(b,d+1)^sayln(",")^
        indent(d+1)^say(t)^say(",")^say(f)^say(")")
    | stm (MOVE (a,b),d) = 
        indent(d)^sayln("MOVE(")^exp(a,d+1)^
        sayln(",")^exp(b,d+1)^say(")")
    | stm (EXP e, d) = indent(d)^sayln("EXP(")^exp(e,d+1)^say(")")

  (* exp : tree.exp * int -> string *)
  and exp (BINOP (p,a,b),d) = 
        indent(d)^say("BINOP(")^printBinOp(p)^sayln(",")^
        exp(a,d+1)^sayln(",")^exp(b,d+1)^say(")")
    | exp (MEM e,d) = indent(d)^sayln("MEM(")^exp(e,d+1)^say(")")
    | exp (TEMP t, d) = indent(d)^say("TEMP ")^say(t)
    | exp (ESEQ (s,e),d) = 
        indent(d)^sayln("ESEQ(")^stm(s,d+1)^sayln(",")^
        exp(e,d+1)^say(")")
    | exp (NAME lab, d) = indent(d)^say("NAME ")^say(lab)
    | exp (CONST i, d) = indent(d)^say("CONST ")^say(Int.toString i)
    | exp (CALL (e,el),d) = 
        indent(d)^sayln("CALL(")^(exp(e,d+1))^
        concat(map (fn a => sayln(",")^exp(a,d+2)) el)^say(")")

  and printBinOp PLUS = say "PLUS"
    | printBinOp MINUS = say "MINUS"
    | printBinOp MUL = say "MUL"
    | printBinOp DIV = say "DIV"
    | printBinOp AND = say "AND"
    | printBinOp OR = say "OR"
    | printBinOp LSHIFT = say "LSHIFT"
    | printBinOp RSHIFT = say "RSHIFT"
    | printBinOp ARSHIFT = say "ARSHIFT"
    | printBinOp XOR = say "XOR"

  and printRelOp EQ = say "EQ"
    | printRelOp NE = say "NE"
    | printRelOp LT = say "LT"
    | printRelOp GT = say "GT"
    | printRelOp LE = say "LE"
    | printRelOp GE = say "GE"
    | printRelOp ULT = say "ULT"
    | printRelOp ULE = say "ULE"
    | printRelOp UGT = say "UGT"
    | printRelOp UGE = say "UGE"

  (* printTreeExp : tree.exp -> string *)
  fun printTreeExp e0 = exp(e0,0)^sayln("")

  (* printTreeStm : tree.stm -> string *)
  fun printTreeStm s0 = stm(s0,0)^sayln("")

end

