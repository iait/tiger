signature codegen = sig

  (* Genera el código assembler para un procedimiento *)
  val codegen : tree.stm list -> assem.instr list

end