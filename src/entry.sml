structure entry = struct

  open ast
  open table
  open typ
  open temp
  open translate

  datatype EnvEntry =
    VIntRO of {access: access, level: int} (* int readonly *)
    | Var of {ty: Tipo, access: access, level: int}
    | Func of {level: level, label: label, formals: Tipo list, result: Tipo, extern: bool}

end
