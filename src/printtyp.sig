signature printtyp = sig

  val printTyp : typ.Tipo -> unit
  val printTEnv : (string, typ.Tipo) table.Tabla -> unit

end