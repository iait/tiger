signature frame = sig

  type frame
  type register = string

  datatype access = InFrame of int (* offset desde el fp *)
                  | InReg of temp.temp

  datatype frag = PROC of {body: tree.stm list, frame: frame}
                | STRING of temp.label * string

  (* Registros especiales *)
  val rv : temp.temp (* return value *)
  val fp : temp.temp (* frame pointer *)
  val sp : temp.temp (* stack pointer *)
  val rax : temp.temp (* para la división *)
  val rdx : temp.temp (* para la división *)

  val wSz : int                  (* tamaño de la palabra *)
  val log2wSz : int              (* log base 2 de word size *)

  (* offsets en bytes a partir del fp *)
  val fpPrev : int               (* ubicación del fp anterior *)
  val fpPrevLev : int            (* ubicación del static link *)

  val argRegs : temp.temp list
  val callerSave : temp.temp list
  val calleeSave : temp.temp list
  val machineRegs : temp.temp list

  (* crea un nuevo frame *)
  val newFrame : {name: temp.label, formals: bool list} -> frame
  (* obtiene el nombre del frame *)
  val name : frame -> string

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
