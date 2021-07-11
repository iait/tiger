signature frame = sig

  type frame
  type register = string

  datatype access = InFrame of int (* offset desde el fp *)
                  | InReg of temp.temp

  datatype frag = PROC of {body: tree.stm, frame: frame}
                | STRING of temp.label * string

  (* Registros especiales *)
  val rv : temp.temp (* return value *)
  val fp : temp.temp (* frame pointer *)
  val sp : temp.temp (* stack pointer *)

  val wSz : int                  (* tamaño de la palabra *)

  (* offsets en bytes a partir del fp *)
  val fpPrev : int               (* ubicación del fp anterior *)
  val fpPrevLev : int            (* ubicación del static link *)

  val argRegs : temp.temp list
  val callDefs : temp.temp list
  val callerSaves : temp.temp list
  val calleeSaves : temp.temp list
  val specialRegs : temp.temp list

  (* crea un nuevo frame *)
  val newFrame : {name: temp.label, formals: bool list} -> frame
  (* obtiene el nombre del frame *)
  val name : frame -> temp.label

  (* crea una lista de accesos para los argumentos de una función *)
  val formals : frame -> access list
  (* crea un acceso para una variable local *)
  val allocLocal : frame -> bool -> access
  (* TODO *)
  val allocArg : frame -> bool -> access

  (* crea un fragmento para un string *)
  val newStringFrag : temp.label -> string -> frag
  
  (* crea la expresión para acceder a una variable *)
  val exp : access -> tree.exp -> tree.exp
  
  (* invoca a una función externa *)
  val externalCall : string * tree.exp list -> tree.exp
  
  (* procedure entry exit *)
  val procEntryExit1 : frame * tree.stm -> tree.stm
  (*val procEntryExit2 : frame * tigerassem.instr list -> tigerassem.instr list*)

end
