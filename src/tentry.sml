(* Type Entry *)
structure tentry = struct

  type unique = unit ref

  datatype Tipo = 
    TUnit
    | TNil
    | TInt
    | TString
    | TArray of Tipo * unique
    | TRecord of (string * Tipo ref * int) list * unique
    | TTipo of string

end
