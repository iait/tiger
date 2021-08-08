signature stack = sig

  type 'a Pila

  val nuevaPila : unit -> 'a Pila
  val nuevaPila1 : 'a -> 'a Pila

  val pushPila : 'a Pila -> 'a -> unit
  val popPila : 'a Pila -> unit
  val topPila : 'a Pila -> 'a

  (* para debug *)
  val pilaToList : 'a Pila -> 'a list

end
