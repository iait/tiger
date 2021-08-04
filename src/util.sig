signature util = sig

  exception Duplicated of string

  (* Convierte una lista de booleanos en un string con sus índices *)
  val boolsToStr : bool list -> string

  (* Verifica si una lista de fields tiene nombres repetidos *)
  val hasDup : ast.field list -> string list

  (* Genera un string con n espacios *)
  val indent : int -> string
  
  (* Genera string para mostrar una lista de strings *)
  val showStrList : string list -> string

end
