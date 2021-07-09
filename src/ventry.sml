(* Value Entry (variable and function) *)
structure ventry = struct

  open ast
  open table
  open tentry
  open temp
  open trans

  datatype EnvEntry =
    VIntRO of {access: access, level: int} (* int readonly *)
    | Var of {ty: Tipo, access: access, level: int}
    | Func of {level: level, label: label, formals: Tipo list, result: Tipo, extern: bool}

end
