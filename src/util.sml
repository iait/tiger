structure util :> util = struct

  open ast
  open Splayset
  
  exception Duplicated of string

  (* Indica si s1 es sufijo de s2 *)
  fun isSuffix s1 s2 = 
    let
      val revert = String.implode o rev o String.explode
    in
      String.isPrefix (revert s1) (revert s2)
    end

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

  (* Indica si el elemento está en la lista *)
  fun inList (l, e) = List.exists (fn e' => e=e') l

  (* Reemplaza cada ocurrencia de un elemento en una lista por otro *)
  fun replace ((a, b), []) = []
    | replace ((a, b), (x::xs)) = 
        if a=x then b::replace ((a, b), xs) else x::replace ((a, b), xs)

end
