signature interp = sig

  val interpret : 
    bool -> (tree.stm list * frame.frame) list -> (temp.label * string) list -> unit

end