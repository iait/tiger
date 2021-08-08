structure regalloc :> regalloc = struct

  type allocation = temp.temp -> string

  open frame
  open assem
  open flow
  open liveness
  open Splayset
  open stack
  open table

  type move = temp * temp

  (* conjuntos de temporarios:
   * low-degree non-move-related
   * low-degree move-related
   * high-degree *)
  type tempSets = temp set * temp set * temp set

  type workspace = {
    inter: interGraph,           (* grafo de interferencias *)
    tempSets: tempSets
    
    (*
    spilledTemps: temp list,     (* enviados a memoria *)
    coalescedTemps: temp list,   (* uno de los temp fusionados *)
    
    coalescedMoves: move list,   (* fusionados *)
    constrainedMoves: move list, (* origen y destino interfiriendo *)
    frozenMoves: move list,      (* no considerados para fusionar *)
    worklistMoves: move list,    (* los que podrían ser fusionados *)
    activeMoves: move list       (* todavía no listos para fusionar *)
    *)
  }

  (* pila de temps que vamos sacando del grafo de interferencias *)
  val selectStack: temp Pila = nuevaPila()

  (* cantidad de colores *)
  val K = List.length machineRegs

  (* toma un elemento del conjunto *)
  val take = Splayset.find (fn _ => true)

  (* muestra el workspace para debug *)
  fun showWorkspace (ws : workspace) =
    let
      val _ = showInterGraph (#inter ws)

      val (ld, mr, hd) = #tempSets ws
      val _ = print ("low-degree non-move-related: "^(setToStr id ld)^"\n")
      val _ = print ("low-degree move-related: "^(setToStr id mr)^"\n")
      val _ = print ("high-degree: "^(setToStr id hd)^"\n")

      val _ = print ("selectStack: "^(listToStr id (pilaToList selectStack))^"\n")

      (*
      val _ = print ("spilledTemps: "^(listToStr id (#spilledTemps ws))^"\n")
      val _ = print ("coalescedTemps: "^(listToStr id (#coalescedTemps ws))^"\n")

      fun movToStr (a,b) = b^"<-"^a
      val _ = print ("coalescedMoves: "^(listToStr movToStr (#coalescedMoves ws))^"\n")
      val _ = print ("constrainedMoves: "^(listToStr movToStr (#constrainedMoves ws))^"\n")
      val _ = print ("frozenMoves: "^(listToStr movToStr (#frozenMoves ws))^"\n")
      val _ = print ("worklistMoves: "^(listToStr movToStr (#worklistMoves ws))^"\n")
      val _ = print ("activeMoves: "^(listToStr movToStr (#activeMoves ws))^"\n")
      *)
    in () end

  (* build : instr list -> bool * bool -> workspace *)
  (* Construye el grafo de interferencias y el workspace inicial *)
  fun build (flow, inter) instrs : workspace =
    let
      (* genera control-flow graph *)
      val fg = instrs2flowGraph instrs
      val _ = 
        if not flow then ()
        else (print "Control-flow graph\n"; showFlowGraph fg; print "------------\n")
      val (ig as {adj,moves,movCount}) = flow2interGraph fg
      (* genera el grafo de interferencias *)
      val ig = flow2interGraph fg
      val _ =
        if not inter then ()
        else (print "Interference graph\n"; showInterGraph ig; print "------------\n")
      (* clasifica los temps en low-degree, move-related, high-degree *)
      fun classify [] (ld, mr, hd) = (ld, mr, hd)
        | classify (t::ts) (ld, mr, hd) =
            let
              val degree = numItems (tabSaca (t, adj))
              val movs = (tabSaca (t, movCount)) handle noExiste => 0
            in
              if degree < K then 
                if movs = 0 then
                  classify ts (t::ld, mr, hd)
                else
                  classify ts (ld, t::mr, hd)
              else
                classify ts (ld, mr, t::hd)
            end
      val (ld, mr, hd) = classify (tabClaves adj) ([], [], [])
    in
      {
        inter = ig,
        
        tempSets = (makeTempSet ld, makeTempSet mr, makeTempSet hd)
        
        (*
        spilledTemps = [],
        coalescedTemps = [],
        
        coalescedMoves = [],
        constrainedMoves = [],
        frozenMoves = [],
        worklistMoves = [],
        activeMoves = moves
        *)
      }
    end

  (* Elimina un nodo low-degree non-move-related.
   * Lo saca del grafo de interferencias y del tempSets, y lo añade al selectStack.
   * Se reubica a cada vecino del nodo que pase de high-degree a low-degree. *)
  fun simplifyTemp t {inter,tempSets=(ld, mr, hd)} =
    let
      (* elimina el nodo del grafo *)
      val inter' = removeNode inter t
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

  fun regalloc (flow, inter) frame instrs =
    let
      val ws = build (flow, inter) instrs
      val _ = showWorkspace ws
      val ws' = simplify ws
      val _ = showWorkspace ws'

      (* TODO continuar *)
      
(***** Main *****)
(*
      fun main() =
        let
          val _ = liveAnalysis()
          val _ = build()
          val _ = makeWorkList()
          fun iter() = 
            if not (isEmpty simplifySet) then (simplify(); iter())
            else if not (isEmpty workListMoves) then (coalesce(); iter())
            else if not (isEmpty freezeSet) then (freeze(); iter())
            else if not (isEmpty spillSet) then (selectSpill(); iter())
            else ()
          val _ = iter()
          val _ = assignColors()
        in
          if not (isEmpty spilledList) 
            then (rewriteProgram spilledList; main())
            else ()
        end
      
      and build() = ()
*)
      
    in
      (instrs, fn s => s)
    end
end
