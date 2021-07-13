signature interp = sig

  val inter : 
    bool -> (tree.stm list * frame.frame) list -> (temp.label * string) list -> unit

end