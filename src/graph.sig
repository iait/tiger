signature graph = sig

  type node = int
  type edge = node * node
  type graph

  val newGraph : unit -> graph

  val listNodes : graph -> node list
  val listEdges : graph -> edge list
  val succ : graph -> node -> node Splayset.set
  val pred : graph -> node -> node Splayset.set

  val addNewNode : graph -> node
  val removeNode : graph -> node -> unit
  val addEdge : graph -> edge -> unit
  val removeEdge : graph -> edge -> unit

  val hasNode : graph -> node -> bool
  val hasEdge : graph -> edge -> bool

  (* imprime el grafo para debug *)
  val showGraph : int -> graph -> unit

end