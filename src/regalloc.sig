signature regalloc = sig

  type allocation = temp.temp -> string

  val regalloc : 
    bool -> frame.frame -> assem.instr list -> assem.instr list * allocation

end
