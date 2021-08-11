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

  (* Genera un string a partir de una lista *)
  fun listToStr f l =
    let
      fun aux [] = ""
        | aux [x] = x
        | aux (x::xs) = x^", "^(aux xs) 
    in
      "["^(aux (List.map f l))^"]"
    end

  (* Genera un string a partir de un set *)
  fun setToStr f s =
    let
      fun aux [] = ""
        | aux [x] = x
        | aux (x::xs) = x^", "^(aux xs) 
    in
      "{"^(aux (List.map f (listItems s)))^"}"
    end

  (* Crea un nuevo set a partir de una lista de temporales *)
  fun makeTempSet [] = empty String.compare
    | makeTempSet [t] = singleton String.compare t
    | makeTempSet ts = addList (empty String.compare, ts)

  (* Función identidad de strings *)
  fun id x = x

  (* Reemplaza un elemento por otro en un conjunto *)
  fun setReplace (s, a, b) = (retrieve (s, a); add (delete (s, a), b))

  (* Indica si un elemento no está en el conjunto *)
  val notIn = not o member

end
