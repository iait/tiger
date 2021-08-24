(* Tree Pretty Print *)
signature treepp = sig

  val printTreeExp : tree.exp -> string
  val printTreeStm : tree.stm -> string

  val printBinOp : tree.binop -> string
  val printRelOp : tree.relop -> string

end