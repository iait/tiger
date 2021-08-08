signature liveness = sig

  type interGraph = {
    adj: (temp.temp, temp.temp Splayset.set) table.Tabla,
    moves: (temp.temp * temp.temp) list,
    movCount: (temp.temp, int) table.Tabla
  }

  val flow2interGraph : flow.flowGraph -> interGraph

  (* imprime el grafo de interferencias para debug *)
  val showInterGraph : interGraph -> unit

end