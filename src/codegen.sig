signature codegen = sig

  (* Genera el código assembler para un procedimiento y lo vuelca en el outstream *)
  val codegen : TextIO.outstream -> bool -> (tree.stm list * frame.frame) -> unit

end