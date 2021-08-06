structure regalloc :> regalloc = struct

  type allocation = temp.temp -> string

  open frame
  open assem
  open flow
  open liveness

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
    in
      (instrs, fn s => s)
    end
end
