signature codegen = sig

  val codegen : frame.frame -> tree.stm -> assem.instr list

  val aux : tree.stm list * frame.frame -> unit

end