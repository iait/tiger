signature liveness = sig

  type move = temp.temp * temp.temp

  type interGraph = {
    adj: (temp.temp, temp.temp Splayset.set) table.Tabla,
    mov: (temp.temp, temp.temp Splayset.set) table.Tabla
  }

  (* Construye un grafo de interferencias a partir del control-flow graph *)
  val flow2interGraph : flow.flowGraph -> interGraph * move list

  (* Crea una copia del grafo de interferencias *)
  val fromInterGraph : interGraph -> interGraph

  (* Agrega una arista *)
  val addEdge : 
    (temp.temp, temp.temp Splayset.set) table.Tabla -> temp.temp * temp.temp -> unit

  (* Elimina una arista *)
  val removeEdge : 
    (temp.temp, temp.temp Splayset.set) table.Tabla -> temp.temp * temp.temp -> unit

  (* Agrega un nodo *)
  val addNode :
    (temp.temp, temp.temp Splayset.set) table.Tabla -> temp.temp -> unit

  (* Elimina un nodo *)
  val removeNode :
    (temp.temp, temp.temp Splayset.set) table.Tabla -> temp.temp -> unit

  (* imprime el grafo de interferencias para debug *)
  val showInterGraph : interGraph -> unit

end