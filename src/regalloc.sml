structure regalloc :> regalloc = struct

  type allocation = temp.temp -> string

  open frame
  open assem
  open flow
  open liveness
  open Splayset
  open stack
  open table
  open temp
  open util

  (* bandera para debug *)
  val debug = ref false

  (* ítem del selectStack *)
  type selectItem = {temp: temp, adjs: temp set, alias: temp option}

  (* pila de temps que vamos sacando del grafo de interferencias *)
  val selectStack: selectItem Pila = nuevaPila()

  (* registros de la máquina *)
  val machineRegs: temp set = makeTempSet (generalRegs @ specialRegs)

  (* registros que se pueden usar para colorear *)
  val colorRegs: temp set = makeTempSet generalRegs

  (* cantidad de colores *)
  val K = numItems colorRegs

  (* genera string para un move *)
  fun showMove (t1,t2) = t2^"<-"^t1

  (* imprime el stack de select para debug *)
  fun showSelectStack() =
    let
      fun showSelectItem {temp,adjs,alias} =
        (print "  ";
        print ("temp: "^temp^", ");
        case alias of
          SOME a => print ("alias: "^a^"\n")
          | NONE => print ("adjs: "^(setToStr id adjs)^"\n"))
    in
      print ("selectStack: \n");
      List.app showSelectItem (pilaToList selectStack)
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
  (* makeAlloc : (temp, string option) Tabla -> allocation *)
  fun makeAlloc table temp = "%"^(valOf (tabSaca (temp, table)))

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
      val _ = pushPila selectStack {temp=t, adjs=adjs, alias=NONE}
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
      val (temp,coalesced) = if member (machineRegs, t1) then (t1,t2) else (t2,t1)
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
      fun updateMoves [] rs = rs
        | updateMoves ((t1,t2)::ms) rs =
        if t1=coalesced then
          if t2=temp then updateMoves ms rs
          else updateMoves ms ((temp,t2)::rs)
        else if t2=coalesced then
          if t1=temp then updateMoves ms rs
          else updateMoves ms ((t1,temp)::rs)
        else
          updateMoves ms ((t1,t2)::rs)
      val ms' = updateMoves ms []
      (* agrega el nodo fusionado al mapa de alias *)
      val _ = pushPila selectStack {temp=coalesced, adjs=makeTempSet[], alias=SOME temp}
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
      val _ = pushPila selectStack {temp=t, adjs=adjs, alias=NONE}
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
              degree < K andalso movs = 0 andalso notIn (machineRegs, t)
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
                if (notIn (machineRegs, t1) orelse notIn (machineRegs, t2))
                andalso notIn (tabSaca (t1, adj), t2) andalso briggs (t1,t2) then
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
                  if notIn (machineRegs, t) andalso moveRelated andalso d' < d then
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
                  if notIn (machineRegs, t) andalso d' > d then
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
          val alloc : (temp, string option) Tabla = tabNueva()
          val _ = Splayset.app (fn r => tabMete (r, SOME r, alloc)) machineRegs
          (* asigna color a un temp distinto al de sus vecinos *)
          fun color ({temp,adjs,alias=SOME a}, spilled) =
                let
                  val _ = printDebug ("## coloring "^temp^" ##\n")
                  val c = tabSaca (a, alloc)
                  val _ = printDebug ("alias: "^a^", color: "^(getOpt (c, "spilled"))^"\n")
                  val _ = tabMete (temp, c, alloc)
                in
                  spilled
                end
            | color ({temp,adjs,alias=NONE}, spilled) =
                let
                  val _ = printDebug ("## coloring "^temp^" ##\n")
                  val neighbors = difference (adjs, spilled)
                  val _ = printDebug ("neighbors: "^(setToStr id adjs)^", ")
                  fun aux (n, cs) = case tabSaca (n, alloc) of
                    SOME c => c::cs
                    | NONE => cs
                  val usedColors = makeTempSet (List.foldl aux [] (listItems neighbors))
                in
                  case Splayset.find (fn c => notIn (usedColors, c)) colorRegs of
                      SOME c => 
                        (printDebug ("color: "^c^"\n");
                        tabMete (temp, SOME c, alloc);
                        spilled)
                    | NONE => 
                        (printDebug "spilled!\n";
                        tabMete (temp, NONE, alloc);
                        add (spilled, temp))
                end
          (* colorea los nodos del stack *)
          val spilled = fold color (makeTempSet[]) selectStack
          (* muestra los temps enviados a memoria para debug *)
          fun showSpilled() = printDebug ("spilled: "^(setToStr id spilled)^"\n")
          (* muestra la tabla alloc para debug *)
          fun showAlloc() =
            let
              val _ = printDebug "allocation:\n";
              fun aux (t, c) =
                if member (machineRegs, t) then ()
                else print ("  "^t^": "^(getOpt (c, "spilled"))^"\n")
            in
              if !debug then List.app aux (tabAList alloc) else ()
            end
        in
          if isEmpty spilled then
            (showAlloc(); (instrs, makeAlloc alloc))
          else
            (showSpilled(); rewrite (listItems spilled) instrs)
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
