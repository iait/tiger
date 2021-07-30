structure graph :> graph = struct

  open table
  open Splayset

  type node = int
  type edge = node * node
  type graph = {
    nodeCount: int ref,
    succ: (node, node set) Tabla,
    pred: (node, node set) Tabla
  }

  (* emptySet : unit -> node set *)
  fun emptySet() = empty Int.compare

  (* newGraph : unit -> graph *)
  fun newGraph() = {
    nodeCount = ref 0,
    succ = tabNueva(),
    pred = tabNueva()
  }

  (* listNodes : graph -> node list *)
  fun listNodes (g: graph) = tabClaves (#succ(g))

  (* listEdges : graph -> edge list *)
  fun listEdges (g: graph) =
    let
      val l : (node * node set) list = tabAList (#succ(g))
    in 
      List.concat (List.map (fn (n, ns) => List.map (fn n' => (n, n')) (listItems ns)) l)
    end

  (* succ : graph -> node -> node set *)
  fun succ (g: graph) n = tabSaca (n, #succ(g))

  (* pred : graph -> node -> node set *)
  fun pred (g: graph) n = tabSaca (n, #pred(g))

  (* addNewNode : graph -> node *)
  fun addNewNode {nodeCount,succ,pred} =
    let
      val n = !nodeCount
      val _ = nodeCount := n+1;
      val _ = tabMete (n, emptySet(), succ)
      val _ = tabMete (n, emptySet(), pred)
    in
      n
    end

  (* removeEdge : graph -> edge -> unit *)
  fun removeEdge {nodeCount,succ,pred} (p, s) =
    let
      (* p no va a tener más como sucesor a s *)
      val _ = case tabBusca (p, succ) of
        SOME ss => tabMete (p, delete (ss, s), succ)
        | NONE => ()
      (* s no va a tener más como predecesor a p *)
      val _ = case tabBusca (s, pred) of
        SOME ps => tabMete (s, delete (ps, p), pred)
        | NONE => () 
    in () end

  (* removeNode : graph -> node -> unit *)
  fun removeNode (g: graph) n =
    let
      val pred : node set = tabElimina (n, #pred(g))
      val succ : node set = tabElimina (n, #succ(g))
    in
      Splayset.app (fn p => removeEdge g (p, n)) pred;
      Splayset.app (fn s => removeEdge g (n, s)) succ
    end

  (* addEdge : graph -> edge -> unit *)
  fun addEdge {nodeCount,succ,pred} (p, s) = (
    tabMete (p, add (tabSaca (p, succ), s), succ);
    tabMete (s, add (tabSaca (s, pred), p), pred)
  )

  (* hasNode : graph -> node -> bool *)
  fun hasNode (g: graph) n = tabEsta (n, #succ(g))

  (* hasEdge : graph -> edge -> bool *)
  fun hasEdge (g: graph) (p, s) = 
    case tabBusca (p, #succ(g)) of
      SOME ss => member (ss, s)
      | NONE => false

end