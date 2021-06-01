structure util :> util = struct

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

end
