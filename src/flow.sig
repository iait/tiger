signature flow = sig

  type flowGraph = {
    control: graph.graph,                                   (* control-flow directed graph *)
    def: (graph.node, temp.temp Splayset.set) table.Tabla,  (* temporarios definidos en cada nodo *)
    use: (graph.node, temp.temp Splayset.set) table.Tabla,  (* temporarios utilizados en cada nodo *)
    isMove: (graph.node, bool) table.Tabla,                 (* si la intrucción es un move *)
    nodes: (graph.node, assem.instr) table.Tabla            (* instrucciones para debug *)
  }

  val instrs2flowGraph : assem.instr list -> flowGraph

  (* imprime el control-flow graph para debug *)
  val showFlowGraph : flowGraph -> unit

end