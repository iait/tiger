structure entry = struct

  open ast
  open table
  open typ
  open temp

  datatype EnvEntry =
    VIntRO	(* int readonly *)
    | Var of {ty: Tipo}
    | Func of {level: unit, label: label, formals: Tipo list, result: Tipo, extern: bool}

  val mainLevel = ()

end
