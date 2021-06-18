signature util = sig

  exception Duplicated of string

  (* Convierte una lista de booleanos en un string con sus índices *)
  val boolsToStr : bool list -> string
  
  (* Verifica si una lista de fields tiene nombres repetidos *)
  val hasDup : ast.field list -> string list

end
