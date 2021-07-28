signature codegen = sig

  val originalCodegen : frame.frame -> tree.stm -> assem.instr list

  val codegen : tree.stm list * frame.frame -> unit

end