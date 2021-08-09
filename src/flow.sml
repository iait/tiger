structure flow :> flow = struct

  open graph
  open Splayset
  open table
  open temp
  open assem
  open util
  open frame

  type flowGraph = {
    control: graph,                 (* control-flow directed graph *)
    def: (node, temp set) Tabla,    (* temporarios definidos en cada nodo *)
    use: (node, temp set) Tabla,    (* temporarios utilizados en cada nodo *)
    isMove: (node, bool) Tabla,     (* si la intrucción es un move *)
    nodes: (node, instr) Tabla,     (* instrucciones para debug *)
    temps: (temp, unit) Tabla       (* todos los temporales *)
  }

  (* instrs2flowGraph : assem.instr list -> flowGraph *)
  fun instrs2flowGraph is =
    let
      (* inicializa el grafo *)
      val fg as {control, def, use, isMove, nodes, temps} : flowGraph =
      {
        control = newGraph(),
        def = tabNueva(),
        use = tabNueva(),
        isMove = tabNueva(),
        nodes = tabNueva(),
        temps = tabNueva()
      }
      (* mapa que asocia los labels con los nodos *)
      val labelMap : (label, node) Tabla = tabNueva()
      (* asocia el nodo con todos los labels de la lista *)
      fun insertNodeLabels n ls = List.app (fn l => tabMete (l, n, labelMap)) ls
      (* añade las instrucciones como nodos al grafo *)
      (* recibe los labels que definen la próxima instrucción y la lista de instrucciones *)
      fun makeNodes ls [] = ls
        | makeNodes ls ((i as OPER {assem,dst,src,jmp})::is) =
            let
              val n = addNewNode control
              val _ = tabMete (n, makeTempSet dst, def)
              val _ = tabMete (n, makeTempSet src, use)
              val _ = tabMete (n, false, isMove)
              val _ = tabMete (n, i, nodes)
              val _ = insertNodeLabels n ls
            in makeNodes [] is end
        | makeNodes ls (LAB {assem,lab}::is) = makeNodes (lab::ls) is
        | makeNodes ls ((i as MOV {assem,dst,src})::is) =
            let
              val n = addNewNode control
              val _ = tabMete (n, makeTempSet [dst], def)
              val _ = tabMete (n, makeTempSet [src], use)
              val _ = tabMete (n, true, isMove)
              val _ = tabMete (n, i, nodes)
              val _ = insertNodeLabels n ls
            in makeNodes [] is end
      (* nodo inicial *)
      val startNode = addNewNode control (* TODO completar def *)
      val _ = tabMete (startNode, makeTempSet [], def)
      val _ = tabMete (startNode, makeTempSet [], use)
      val _ = tabMete (startNode, false, isMove)
      val _ = tabMete (startNode, OPER {assem="",dst=[],src=[],jmp=[]}, nodes)
      (* crea los nodos y llena el mapa de labels *)
      val ls = makeNodes [] is
      (* nodo final *)
      val endNode = addNewNode control (* TODO completar use *)
      val _ = tabMete (endNode, makeTempSet [], def)
      val _ = tabMete (endNode, makeTempSet [rax], use)
      val _ = tabMete (endNode, false, isMove)
      val _ = tabMete (endNode, OPER {assem="",dst=[],src=[],jmp=[]}, nodes)
      val _ = insertNodeLabels endNode ls
      (* añade una arista dado un nodo y un label
       * el nodo sucesor se buscará en labelMap *)
      fun addNodeLabelEdge n l = addEdge control (n, tabSaca (l, labelMap))
      (* completa las aristas *)
      fun makeEdges _ [] = ()
        | makeEdges prev ((n, OPER {assem,dst,src,jmp})::ns) =
            let
              val _ = List.app (fn t => tabMete (t, (), temps)) dst
              val _ = List.app (fn t => tabMete (t, (), temps)) src
              val _ = case prev of
                NONE => ()
                | SOME p => addEdge control (p, n)
              val prev' = case jmp of
                [] => SOME n
                | _ => (List.app (addNodeLabelEdge n) jmp; NONE)
            in makeEdges prev' ns end
        | makeEdges prev ((n, MOV {assem,dst,src})::ns) =
            let
              val _ = tabMete (dst, (), temps)
              val _ = tabMete (src, (), temps)
              val _ = case prev of
                NONE => ()
                | SOME p => addEdge control (p, n)
            in makeEdges (SOME n) ns end
        | makeEdges _ _ = raise Fail "no debería llegar LAB"
      val _ = makeEdges NONE (tabAList nodes)
    in fg end

  (* imprime el control-flow graph para debug *)
  fun showFlowGraph ({control,def,use,isMove,nodes,temps} : flowGraph) =
    let
      val showNode = Int.toString
      fun showTempSet s =
        let
          fun aux [] = ""
            | aux [t] = t
            | aux (t::ts) = t^", "^(aux ts)
        in "{"^(aux (listItems s))^"}" end
    
      val _ = (print "control:\n"; showGraph 2 control)
      val _ = (print "def:\n"; showTabla (2, showNode, showTempSet, def))
      val _ = (print "use:\n"; showTabla (2, showNode, showTempSet, use))
      val _ = (print "isMove:\n"; showTabla (2, showNode, Bool.toString, isMove))
      val _ = (print "nodes:\n"; showTabla (2, showNode, showInstr, nodes))
      val _ = (print "temps: "; print (listToStr id (tabClaves temps)); print "\n")
    in () end 

end