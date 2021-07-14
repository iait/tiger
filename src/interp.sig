signature interp = sig

  val interp : 
    bool -> (tree.stm list * frame.frame) list -> (temp.label * string) list -> unit

end