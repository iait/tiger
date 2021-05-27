signature escape = sig

  (* Recorre el AST y completa los escapes *)
  val findEscape: ast.exp -> unit

end
