signature frame = sig

  type frame
  type register = string

  datatype access = InFrame of int 
                  | InReg of temp.temp
                  | todo

  datatype frag = PROC of {body: tree.stm, frame: frame}
                | STRING of temp.label * string

  (* Algunos registros especiales *)
  val rv : temp.temp
  val ov : temp.temp
  val fp : temp.temp
  val sp : temp.temp

  (* Algunas constantes útiles *)
  val fpPrev : int
  val fpPrevLev : int
  val wSz : int
  val log2WSz : int
  val callDefs : temp.temp list
  val callerSaves : temp.temp list
  val calleeSaves : temp.temp list

  val newFrame : {name: temp.label, formals: bool list} -> frame
  val name : frame -> temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
  val allocArg : frame -> bool -> access

  val string : temp.label * string -> string
  val exp : access -> tree.exp -> tree.exp
  val externalCall : string * tree.exp list -> tree.exp
  val procEntryExit1 : frame * tree.stm -> tree.stm
  (*val procEntryExit2 : frame * tigerassem.instr list -> tigerassem.instr list*)

end
