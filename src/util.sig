signature util = sig

  exception Duplicated of string

  (* Convierte una lista de booleanos en un string con sus índices *)
  val boolsToStr : bool list -> string

  (* Verifica si una lista de fields tiene nombres repetidos *)
  val hasDup : ast.field list -> string list

  (* Genera un string con n espacios *)
  val indent : int -> string

  (* Genera un string a partir de una lista *)
  val 'a listToStr : ('a -> string) -> 'a list -> string

  (* Genera un string a partir de un set *)
  val 'a setToStr : ('a -> string) -> 'a Splayset.set -> string

  (* Crea un nuevo set a partir de una lista de temporales *)
  val makeTempSet : temp.temp list -> temp.temp Splayset.set

  (* Función identidad *)
  val 'a id : 'a -> 'a

end
