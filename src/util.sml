structure util :> util = struct

  open ast
  open Splayset
  
  exception Duplicated of string

  local
    fun aux1 _ [] = []
      | aux1 n (b::bs) = if b then aux1 (n+1) bs else n::(aux1 (n+1) bs)
    fun aux2 [] = ""
      | aux2 [x] = Int.toString x
      | aux2 [x, y] = (Int.toString x)^" y "^(Int.toString y)
      | aux2 (x::y::xs) =  (Int.toString x)^", "^(aux2 (y::xs))
  in
    val boolsToStr = aux2 o (aux1 1)
  end
  
  val hasDup = List.foldl (fn ({name,escape,typ}, ns) => 
    if List.exists (fn x => x=name) ns
      then raise Duplicated name
      else (name::ns)) []

  (* Genera un string con n espacios *)
  fun indent n = String.concat (List.tabulate (n, fn _ => " "))

  (* Genera string para mostrar una lista de strings *)
  fun showStrList l =
    let
      fun aux [] = ""
        | aux [x] = x
        | aux (x::xs) = x^", "^(aux xs)
    in "["^(aux l)^"]" end;

  (* crea un nuevo set a partir de una lista de temporales *)
  fun makeTempSet [] = empty String.compare
    | makeTempSet [t] = singleton String.compare t
    | makeTempSet ts = addList (empty String.compare, ts)

end
