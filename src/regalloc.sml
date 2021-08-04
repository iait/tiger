structure regalloc :> regalloc = struct

  type allocation = temp.temp -> string

  open frame
  open assem
  open flow

  fun regalloc (flow, inter) frame instrs =
    let

    in
      (instrs, fn s => s)
    end
end
