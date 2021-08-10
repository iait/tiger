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
  val selectStack: temp Pila = nuevaPila()

  (* pila de subgrafos de interferencia *)
  val graphStack: interGraph Pila = nuevaPila()

  (* cantidad de colores *)
  val K = List.length machineRegs

  (* genera string para un move *)
  fun showMove (t1,t2) = t2^"<-"^t1

  (* imprime el stack de select para debug *)
  fun showSelectStack() =
    print ("selectStack: "^(listToStr id (pilaToList selectStack))^"\n")

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

  (* Arma la función de coloreo a partir de una tabla *)
  (* makeAlloc : (temp, string) Tabla -> allocation *)
  fun makeAlloc table = fn t => tabSaca (t, table)

  (* Elimina un nodo low-degree non-move-related.
   * Lo saca del grafo de interferencias y lo añade al selectStack. *)
  (* simplifyTemp : temp -> interGraph -> interGraph *)
  fun simplifyTemp t inter =
    let
      (* crea una copia del grafo *)
      val inter' as {adj,mov} = fromInterGraph inter
      (* elimina el nodo del grafo *)
      val _ = removeNodeWithEdges adj t
      val _ = removeNode mov t
      (* mete el temporal t a la pila y el grafo original *)
      val _ = pushPila selectStack t
      val _ = pushPila graphStack inter
    in
      inter'
    end

  (* Fusiona un move en el grafo de interferencias *)
  (* coalesceMov : move -> interGraph -> interGraph *)
  fun coalesceMov (t1, t2) inter =
    let
      (* crea una copia del grafo *)
      val inter' as {adj,mov} = fromInterGraph inter
      (* cada vecino de t2 ahora será vecino de t1 *)
      val adj2 = tabSaca (t2, adj)
      val _ = Splayset.app (fn n => addEdge adj (n, t1)) adj2
      (* elimina el temporario t2 *)
      val _ = removeNodeWithEdges adj t2
      (* TODO agregar a t1 como alias de t2 *)
      (* los temps adyacentes a t1 y a t2 disminuirán en 1 su grado *)
      (* el nodo resultante t1 puede dejar de ser move-related *)
    in
      inter'
    end

  (* Reduce todos los nodos del grafo *)
  (* reduce : interGraph * move list -> unit *)
  fun reduce (ig, ms) =
    let
      (* Simplifica nodos low-degree non-move-related *)
      (* simplify : interGraph * move list -> unit *)
      fun simplify (inter as {adj,mov}, ms) =
        let
          (* indica si el nodo puede ser simplificado: low-degree non-move-related *)
          fun canSimplify t =
            let
              val degree = numItems (tabSaca (t, adj))
              val movs = numItems (tabSaca (t, mov))
            in
              degree < K andalso movs = 0
            end
        in
          case List.find canSimplify (tabClaves adj) of
            NONE => coalesce (inter, ms)
            | SOME t => simplify (simplifyTemp t inter, ms)
        end

      (* Fusiona los dos nodos de un mov de forma segura *)
      (* coalesce : interGraph * move list -> unit *)
      and coalesce (ig as {adj,mov}, ms) =
        let
          val _ = printInterGraph (ig, ms)
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
                if not (member (tabSaca (t1, adj), t2)) andalso briggs (t1,t2) then
                  SOME ((t1,t2), rs @ ms)
                else
                  findMov ms ((t1,t2)::rs)
        in
          case findMov ms [] of
            NONE => freeze (ig, ms)
            | SOME (m, ms') => simplify (coalesceMov m ig, ms')
        end

      and freeze (ig as {adj,mov}, ms) =
        let
          (* TODO *)
          val _ = printInterGraph (ig, ms)
          val _ = List.app (fn t => pushPila selectStack t) (tabClaves adj)
        in
          ()
        end
      
      and spill() = ()
    in
      simplify (ig, ms)
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
          val tabla : (temp, string) Tabla = tabNueva()
          val _ = List.app (fn t => tabMete (t, t, tabla)) (pilaToList selectStack)
          (* TODO *)
          val spilled = []
        in
          if spilled = [] then
            (instrs, makeAlloc tabla)
          else
            rewrite spilled instrs
        end

      (* Reescribe el programa volcando a memoria los temporarios spilled *)
      (* rewrite : temp list -> instr list -> instr list * allocation *)
      and rewrite spilled instrs =
        let
          (* TODO *)
          val instrs' = instrs
        in
          build instrs'
        end

    in
      build instrs
    end
end
