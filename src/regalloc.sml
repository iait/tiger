structure regalloc :> regalloc = struct

  type allocation = temp.temp -> string

  open frame
  open assem
  open flow
  open liveness
  open Splayset
  open stack
  open table

  (* conjuntos de temporarios:
   *  low-degree non-move-related
   *  low-degree move-related
   *  high-degree *)
  type tempSets = temp set * temp set * temp set

  type workspace = {
    inter: interGraph,           (* grafo de interferencias *)
    tempSets: tempSets
  }

  (* pila de temps que vamos sacando del grafo de interferencias *)
  val selectStack: temp Pila = nuevaPila()

  (* cantidad de colores *)
  val K = List.length machineRegs

  (* toma un elemento del conjunto *)
  val take = Splayset.find (fn _ => true)

  (* genera string para un move *)
  fun showMove (t1,t2) = t2^"<-"^t1

  (* muestra el workspace para debug *)
  fun showWorkspace (ws : workspace) =
    let
      val _ = showInterGraph (#inter ws)

      val (ld, mr, hd) = #tempSets ws
      val _ = print ("low-degree non-move-related: "^(setToStr id ld)^"\n")
      val _ = print ("low-degree move-related: "^(setToStr id mr)^"\n")
      val _ = print ("high-degree: "^(setToStr id hd)^"\n")

      val _ = print ("selectStack: "^(listToStr id (pilaToList selectStack))^"\n")
    in () end

  (* getMoves : (temp, temp set) Tabla -> move list *)

  (* build : instr list -> bool * bool -> workspace *)
  (* Construye el grafo de interferencias y el workspace inicial *)
  fun build (flow, inter) instrs : workspace =
    let
      (* genera control-flow graph *)
      val fg = instrs2flowGraph instrs
      val _ = 
        if not flow then ()
        else (print "Control-flow graph\n"; 
          showFlowGraph fg; 
          print "-------------------------\n")
      (* genera el grafo de interferencias *)
      val (ig, ms) = flow2interGraph fg
      val {adj,mov} = ig
      val _ =
        if not inter then ()
        else (print "Interference graph\n"; 
          showInterGraph ig;
          print ("moves: "^(listToStr showMove ms)^"\n");
          print "-------------------------\n")
      (* clasifica los temps en low-degree, move-related, high-degree *)
      fun classify [] (ld, mr, hd) = (ld, mr, hd)
        | classify (t::ts) (ld, mr, hd) =
            let
              val degree = numItems (tabSaca (t, adj))
              val moveRelated = numItems (tabSaca (t, mov)) > 0
            in
              if degree < K then 
                if moveRelated then
                  classify ts (ld, t::mr, hd)
                else
                  classify ts (t::ld, mr, hd)
              else
                classify ts (ld, mr, t::hd)
            end
      val (ld, mr, hd) = classify (tabClaves adj) ([], [], [])
    in
      {
        inter = ig,
        tempSets = (makeTempSet ld, makeTempSet mr, makeTempSet hd)
      }
    end

(* refactorizar

  (* Elimina un nodo low-degree non-move-related.
   * Lo saca del grafo de interferencias y del tempSets, y lo añade al selectStack.
   * Se reubica a cada vecino del nodo que pase de high-degree a low-degree. *)
  fun simplifyTemp t {inter={adj,mov}, tempSets=(ld, mr, hd)} =
    let
      (* elimina el nodo del grafo *)
      val inter' = removeNode adj t
      (* mete el temporal t a la pila *)
      val _ = pushPila selectStack t
      (* reclasifica el nodo n vecino de t *)
      fun reclassify (n, (ld, mr, hd)) =
        let
          val degree = numItems (tabSaca (n, #adj inter'))
          val movs = (tabSaca (t, #movCount inter')) handle noExiste => 0
        in
          if degree = K-1 then
            if movs = 0 then
              (add (ld, n), mr, delete (hd, n))
            else
              (ld, add (mr, n), delete (hd, n))
          else
            (ld, mr, hd)
        end
      (* reclasifica todos los vecinos de t *)
      val neighbors = tabSaca (t, #adj inter)
      val tempSets' = Splayset.foldl reclassify (delete (ld, t), mr, hd) neighbors
    in
      {inter = inter', tempSets = tempSets'}
    end

  (* simplify : workspace -> workspace *)
  (* simplifica los nodos low-degree non-move-related hasta que no quede ninguno *)
  fun simplify (ws as {inter,tempSets=(ld, mr, hd)}) =
    case take ld of
      NONE => ws
      | SOME t => simplify (simplifyTemp t ws)

  (**)
  fun coalesceMov (t1, t2) rs (ws as {inter,tempSets}) =
    let
      val {adj,moves,movCount} = inter
      val adj1 = tabSaca (t1, adj)
      val adj2 = tabSaca (t2, adj)
      (* elimina el temporario t2 *)
      val (inter' as {adj',moves',movCount'}) = removeNode inter t2
      (* agrega los vecinos de t2 a los vecinos de t1 *)
      val _ = tabMete (t1, union (adj1, adj2), adj')
      (* en el conjunto de adyacencias de cada vecino de t2 reemplaza t2 por t1 *)
      fun replace n = tabMete (n, setReplace (tabSaca (n, adj'), t2, t1), adj')
      val _ = Splayset.app remplace adj2
    (* agregar a adj1 adj2
       a cada n in adj2 reemplazar t2 por t1
       eliminar t2
       agregar alias t2 -> t1 *)
    (* recalcular los tempSets *)

  (* Busca y fusiona dos temporarios de un mov, retornando el ws nuevo.
   * En caso de que no pueda hacerse, retorna NONE. *)
  fun coalesce (ws as {inter,tempSets}) =
    let
      val {adj,moves,movCount} = inter
      (* verifica si el mov se puede fusionar con Briggs *)
      fun briggs (t1,t2) =
        let
          val adj1 = tabSaca (t1, adj)
          val adj2 = tabSaca (t2, adj)
          val intersec = intersection adj1 adj2
          fun degree n = 
            if member (intersec, n) then (numItems (tabSaca (n, adj))) - 1
            else numItems (tabSaca (n, adj))
          val adjs = List.filter (fn n => degree >= K) (itemList (union (adj1, adj2)))
        in
          length adjs < K
        end
      (* devuelve un mov que sea seguro fusionar *)
      fun findMov [] rs = NONE
        | findMov ((t1,t2)::ms) rs =
            if not (member (tabSaca (t1, adj), t2) andalso briggs (t1,t2) then
              SOME ((t1,t2), rs @ ms)
            else findMov ms ((t1,t2)::rs)
      (* busca un mov para fusionar *)
      in
        case findMov moves of
          NONE => NONE
          | SOME (m, rs) => coalesceMov m rs ws
      end

*)

  fun regalloc (flow, inter) frame instrs =
    let
      val ws = build (flow, inter) instrs
      val _ = showWorkspace ws
      (*
      val ws' = simplify ws
      val _ = showWorkspace ws'
      *)
      
    in
      (instrs, fn s => s)
    end
end
