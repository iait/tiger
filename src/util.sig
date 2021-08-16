signature util = sig

  exception Duplicated of string

  (* Verifica si es sufijo *)
  val isSuffix : string -> string -> bool

  (* Convierte una lista de booleanos en un string con sus índices *)
  val boolsToStr : bool list -> string

  (* Verifica si una lista de fields tiene nombres repetidos *)
  val hasDup : ast.field list -> string list

  (* Genera un string con n espacios *)
  val indent : int -> string

  (* Genera un string a partir de una lista *)
  val listToStr : ('a -> string) -> 'a list -> string

  (* Genera un string a partir de un set *)
  val setToStr : ('a -> string) -> 'a Splayset.set -> string

  (* Crea un nuevo set a partir de una lista de temporales *)
  val makeTempSet : temp.temp list -> temp.temp Splayset.set

  (* Función identidad *)
  val id : 'a -> 'a

  (* Reemplaza un elemento por otro en un conjunto *)
  val setReplace : ('a Splayset.set * 'a * 'a) -> 'a Splayset.set

  (* Indica si un elemento no está en el conjunto *)
  val notIn : 'a Splayset.set * 'a -> bool

end
