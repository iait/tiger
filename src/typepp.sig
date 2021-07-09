(* Type Pretty Print *)
signature typepp = sig

  val printTyp : tentry.Tipo -> unit
  val printTEnv : (string, tentry.Tipo) table.Tabla -> unit

end