structure regalloc :> regalloc = struct

  type allocation = temp.temp -> string

  open frame
  open assem
  open flow
  open liveness
  open Splayset
  open stack

  type move = temp * temp

  type workspace = {
    inter: interGraph,           (* grafo de interferencias *)
    
    simplifyWorkList: temp list, (* low-degree non-move-related *)
    freezeWorkList: temp list,   (* low-degree move-related *)
    spillWorkList: temp list,    (* high-degree *)
    spilledNodes: temp list,     (* enviados a memoria *)
    coalescedNodes: temp list,   (* uno de los temp fusionados *)
    seleckStack: temp Pila,      (* eliminados del grafo *)
    
    coalescedMoves: move list,    (* fusionados *)
    constrainedMoves: move list,  (* origen y destino interfiriendo *)
    frozenMoves: move list,       (* no considerados para fusionar *)
    worklistMoves: move list,     (* los que podrían ser fusionados *)
    activeMoves: move list        (* todavía no listos para fusionar *)
  }

  (* build : instr list -> workspace *)
  fun build instrs : workspace =
    let
      val fg = instrs2flowGraph instrs
      val ig = flow2interGraph fg
    in
      {
        inter = ig,
        simplifyWorkList = [],
        freezeWorkList = [],
        spillWorkList = [],
        spilledNodes = [],
        coalescedNodes = [],
        seleckStack = nuevaPila(),
        coalescedMoves = [],
        constrainedMoves = [],
        frozenMoves = [],
        worklistMoves = [],
        activeMoves = []
      }
    end

  fun regalloc (flow, inter) frame instrs =
    let
      (* genera control-flow graph *)
      val fg = instrs2flowGraph instrs
      val _ = 
        if not flow then ()
        else (print "Control-flow graph\n"; showFlowGraph fg; print "------------\n")
      (* genera el grafo de interferencias *)
      val ig = flow2interGraph fg
      val _ =
        if not inter then ()
        else (print "Interference graph\n"; showInterGraph ig; print "------------\n")
      (* TODO continuar *)
      
(***** Main *****)
(*
      fun main() =
        let
          val _ = liveAnalysis()
          val _ = build()
          val _ = makeWorkList()
          fun iter() = 
            if not (isEmpty simplifyWorkList) then (simplify(); iter())
            else if not (isEmpty workListMoves) then (coalesce(); iter())
            else if not (isEmpty freezeWorkList) then (freeze(); iter())
            else if not (isEmpty spillWorkList) then (selectSpill(); iter())
            else ()
          val _ = iter()
          val _ = assignColors()
        in
          if not (isEmpty spilledNodes) 
            then (rewriteProgram spilledNodes; main())
            else ()
        end
      
      and build() = ()
*)
      
    in
      (instrs, fn s => s)
    end
end
