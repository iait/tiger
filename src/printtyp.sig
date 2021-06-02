signature printtyp = sig

  val printTyp : (string, Tipo) table.Tabla -> string -> unit
  val printTEnv : (string, Tipo) table.Tabla -> unit

end