structure regalloc :> regalloc = struct

  type allocation = temp.temp -> string

  open frame
  open assem
  open flow
  open liveness
  open Splayset
  open stack
  open table

  (* bandera para debug *)
  val debug = ref false

  (* pila de temps que vamos sacando del grafo de interferencias *)
  val selectStack: (temp * temp set) Pila = nuevaPila()

  (* mapa con los alias de los temps que se van fusionando *)
  val alias: (temp, temp) Tabla = tabNueva()

  (* cantidad de colores *)
  val K = List.length machineRegs

  (* nodos precoloreados *)
  val precolored: temp set = makeTempSet machineRegs

  (* genera string para un move *)
  fun showMove (t1,t2) = t2^"<-"^t1

  (* imprime el stack de select para debug *)
  fun showSelectStack() =
    let
      fun showSelectTemp (temp,neighbors) =
        (print "  ";
        print ("temp: "^temp^", ");
        print ("neighbors: "^(setToStr id neighbors)^"\n"))
    in
      print ("selectStack: \n");
      List.app showSelectTemp (pilaToList selectStack);
      print ("alias: \n");
      showTabla (2, id, id, alias)
    end

  (* imprime grafo de interferencias si está la bandera de debug activada *)
  fun printInterGraph (ig, ms) =
    if !debug then
      (print "Interference graph\n"; 
      showInterGraph ig;
      print ("moves: "^(listToStr showMove ms)^"\n");
      showSelectStack();
      print "-------------------------\n")
    else ()

  (* imprime el control-flow graph si está la bandera de debug activada *)
  fun printFlowGraph fg =
    if !debug then
      (print "Control-flow graph\n"; 
      showFlowGraph fg; 
      print "-------------------------\n")
    else ()

  (* imprime el mensaje si está la bandera de debug activada *)
  fun printDebug msg = if !debug then print msg else ()

  (* Arma la función de coloreo a partir de una tabla *)
  (* makeAlloc : (temp, string) Tabla -> allocation *)
  fun makeAlloc table = fn t => "%"^(tabSaca (t, table))

  (* Elimina de ms todos los move relacionados al nodo t *)
  fun filterMoves t ms = List.filter (fn (t1,t2) => t1<>t andalso t2<>t) ms

  (* Elimina un nodo low-degree non-move-related.
   * Lo saca del grafo de interferencias y lo añade al selectStack. *)
  (* simplifyTemp : temp -> interGraph * move list -> move list *)
  fun simplifyTemp t (ig as {adj,mov}, ms) =
    let
      val _ = printDebug ("## simplifying "^t^" ##\n")
      (* vecinos de t antes de eliminarlo *)
      val adjs = tabSaca (t, adj)
      (* elimina el nodo del grafo *)
      val _ = removeNodeWithEdges adj t
      val _ = removeNode mov t
      (* mete el temporal t su lista de adyacentes a la pila *)
      val _ = pushPila selectStack (t, adjs)
      (* debug *)
      val _ = printInterGraph (ig, ms)
    in
      ms
    end

  (* Fusiona un move en el grafo de interferencias *)
  (* coalesceMov : move -> interGraph * move list -> move list *)
  fun coalesceMov (t1, t2) (ig as {adj,mov}, ms) =
    let
      (* elige el temporario que queda y el fusionado *)
      val (temp,coalesced) = if member (precolored, t1) then (t1,t2) else (t2,t1)
      val _ = printDebug ("## coalescing "^coalesced^" into "^temp^" ##\n")
      (* cada vecino de coalesced ahora será vecino de temp *)
      val adjs = tabSaca (coalesced, adj)
      val _ = Splayset.app (fn n => addEdge adj (n, temp)) adjs
      val movs = tabSaca (coalesced, mov)
      val _ = Splayset.app (fn n => addEdge mov (n, temp)) movs
      (* elimina el temporario coalesced *)
      val _ = removeNodeWithEdges adj coalesced
      val _ = removeNodeWithEdges mov coalesced
      (* actualiza los moves con el nuevo alias *)
      fun updateMove (t1,t2) =
        if t1=coalesced then (temp,t2)
        else if t2=coalesced then (t1,temp)
        else (t1,t2)
      val ms' = List.map updateMove ms
      (* agrega el nodo fusionado al mapa de alias *)
      val _ = tabMete (coalesced, temp, alias)
      (* debug *)
      val _ = printInterGraph (ig, ms')
    in
      ms'
    end

  (* Abandona el intento de fusionar un nodo move-related, 
   * convirtiéndolo a non-move-related *)
  (* freezeTemp : temp -> interGraph * move list -> move list *)
  fun freezeTemp t (ig as {adj,mov}, ms) =
    let
      val _ = printDebug ("## freezing "^t^" ##\n")
      (* elimina las aristas en el grafo de mov *)
      val _ = Splayset.app (fn n => removeEdge mov (t,n)) (tabSaca (t,mov))
      (* elimina de ms todos los move relacionados al nodo t *)
      val ms' = filterMoves t ms
      (* debug *)
      val _ = printInterGraph (ig, ms')
    in
      ms'
    end

  (* Simplemente saca el nodo del grafo de interferencias pues 
   * se convertirá en un candidato para volcarlo en memoria *)
  (* spillTemp : temp -> interGraph * move list -> move list *)
  fun spillTemp t (ig as {adj,mov}, ms) =
    let
      val _ = printDebug ("## spilling "^t^" ##\n")
      (* vecinos de t *)
      val adjs = tabSaca (t, adj)
      (* elimina el nodo de los grafos adj y mov *)
      val _ = removeNodeWithEdges adj t
      val _ = removeNodeWithEdges mov t
      (* elimina de ms todos los move relacionados al nodo t *)
      val ms' = filterMoves t ms
      (* mete el temporal t su lista de adyacentes a la pila *)
      val _ = pushPila selectStack (t, adjs)
      (* debug *)
      val _ = printInterGraph (ig, ms')
    in
      ms'
    end

  (* Reduce todos los nodos del grafo *)
  (* reduce : interGraph * move list -> unit *)
  fun reduce (ig as {adj,mov}, ms) =
    let
      (* Simplifica nodos low-degree non-move-related *)
      (* simplify : move list -> unit *)
      fun simplify ms =
        let
          (* indica si el nodo puede ser simplificado: low-degree non-move-related *)
          fun canSimplify t =
            let
              val degree = numItems (tabSaca (t, adj))
              val movs = numItems (tabSaca (t, mov))
            in
              degree < K andalso movs = 0 andalso notIn (precolored, t)
            end
        in
          case List.find canSimplify (tabClaves adj) of
            NONE => coalesce ms
            | SOME t => simplify (simplifyTemp t (ig, ms))
        end

      (* Fusiona los dos nodos de un mov de forma segura *)
      (* coalesce : move list -> unit *)
      and coalesce ms =
        let
          (* verifica si el mov se puede fusionar con Briggs *)
          fun briggs (t1,t2) =
            let
              val adj1 = tabSaca (t1, adj)
              val adj2 = tabSaca (t2, adj)
              val intersec = intersection (adj1, adj2)
              fun degree n = 
                if member (intersec, n) then (numItems (tabSaca (n, adj))) - 1
                else numItems (tabSaca (n, adj))
              val adjs = 
                List.filter (fn n => (degree n) >= K) (listItems (union (adj1, adj2)))
            in
              length adjs < K
            end
          (* devuelve un mov que sea seguro fusionar *)
          fun findMov [] rs = NONE
            | findMov ((t1,t2)::ms) rs =
                if notIn (tabSaca (t1, adj), t2) andalso briggs (t1,t2) then
                  SOME ((t1,t2), rs @ ms)
                else
                  findMov ms ((t1,t2)::rs)
        in
          case findMov ms [] of
            NONE => freeze ms
            | SOME (m, ms') => simplify (coalesceMov m (ig, ms'))
        end

      and freeze ms =
        let
          (* devuelve el nodo move-related de menor grado no precoloreado *)
          fun findFreeze [] (n, _) = n
            | findFreeze (t::ts) (n, d) =
                let
                  val d' = numItems (tabSaca (t, adj))
                  val moveRelated = numItems (tabSaca (t, mov)) > 0
                in
                  if notIn (precolored, t) andalso moveRelated andalso d' < d then
                    findFreeze ts (SOME t, d')
                  else
                    findFreeze ts (n, d)
                end
        in
          case findFreeze (tabClaves adj) (NONE, valOf (Int.maxInt)) of
            NONE => spill ms
            | SOME t => simplify (freezeTemp t (ig, ms))
        end
      
      and spill ms =
        let
          (* devuelve el nodo de mayor grado no precoloreado *)
          fun findSpill [] (n, _) = n
            | findSpill (t::ts) (n, d) =
                let
                  val d' = numItems (tabSaca (t, adj))
                in
                  if notIn (precolored, t) andalso d' > d then
                    findSpill ts (SOME t, d')
                  else
                    findSpill ts (n, d)
                end
        in
          case findSpill (tabClaves adj) (NONE, 0) of
            NONE => print "finish!\n" (* solo quedan nodos precoloreados *)
            | SOME t => simplify (spillTemp t (ig, ms))
        end

    in
      simplify ms
    end

  (* Asignación de registros *)
  (* regalloc : bool -> frame -> instr list -> instr list * allocation *)
  fun regalloc d frame instrs =
    let
      val _ = debug := d

      (* Construye el grafo de interferencias *)
      (* build : instr list -> instr list * allocation *)
      fun build instrs =
        let
          (* genera control-flow graph *)
          val fg = instrs2flowGraph instrs
          val _ = printFlowGraph fg
          (* genera el grafo de interferencias *)
          val (ig, ms) = flow2interGraph fg
          val _ = printInterGraph (ig, ms)
          (* reduce el grafo de interferencias *)
          val _ = reduce (ig, ms)
        in
          select instrs
        end
      
      (* Reconstruye el grafo asignando color a los temporarios.
       * Si en el proceso hay temporarios que tienen que ir a memoria,
       * reescribe el programa *)
      (* select : instr list -> instr list * allocation *)
      and select instrs =
        let
          val _ = printDebug "Select\n"
          val alloc : (temp, string) Tabla = tabNueva()
          val _ = Splayset.app (fn r => tabMete (r, r, alloc)) precolored
          (* asigna color a un temp distinto al de sus vecinos *)
          fun color ((t, adjs), spilled) =
            let
              val _ = printDebug ("## coloring "^t^" ##\n")
              val neighbors = difference (adjs, spilled)
              val _ = printDebug ("neighbors: "^(setToStr id adjs)^"\n")
              val usedColors =
                makeTempSet (List.map (fn t => tabSaca (t,alloc)) (listItems neighbors))
            in
              case Splayset.find (fn c => notIn (usedColors, c)) precolored of
                NONE => (printDebug "spilled!\n"; add (spilled, t))
                | SOME c => (printDebug ("color: "^c^"\n"); tabMete (t, c, alloc); spilled)
            end
          (* colorea los nodos del stack *)
          val spilled = fold color (makeTempSet[]) selectStack
          (* asigna el color de su alias a los nodos fusionados *)
          val _ = tabConsume (fn (t,a) => tabMete (t, tabSaca (a, alloc), alloc)) alias
          (* resultado de select para debug *)
          val _ = printDebug "allocation:\n"
          val _ = if !debug then showTabla (2, id, id, alloc) else ()
          val _ = printDebug ("spilled: "^(setToStr id spilled)^"\n")
        in
          if isEmpty spilled then
            (instrs, makeAlloc alloc)
          else
            rewrite (listItems spilled) instrs
        end

      (* Reescribe el programa volcando a memoria los temporarios spilled *)
      (* rewrite : temp list -> instr list -> instr list * allocation *)
      and rewrite spilled instrs =
        let
          (* TODO *)
          val instrs' = instrs
          val _ = raise Fail "Rewrite todavía no implementado!"
        in
          build instrs'
        end

    in
      build instrs
    end
end
