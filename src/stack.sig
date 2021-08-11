signature stack = sig

  type 'a Pila

  val nuevaPila : unit -> 'a Pila
  val nuevaPila1 : 'a -> 'a Pila
  val nuevaPilaList : 'a list -> 'a Pila

  val pushPila : 'a Pila -> 'a -> unit
  val popPila : 'a Pila -> unit
  val topPila : 'a Pila -> 'a
  val retrievePila : 'a Pila -> 'a

  val pilaToList : 'a Pila -> 'a list

  val fold : ('a * 'b -> 'b) -> 'b -> 'a Pila -> 'b

end
