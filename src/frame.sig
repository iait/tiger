signature frame = sig

  type frame

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
  val generalRegs : temp.temp list
  val specialRegs : temp.temp list

  (* crea un nuevo frame *)
  val newFrame : {name: temp.label, formals: bool list} -> frame

  (* obtiene el nombre del frame *)
  val name : frame -> string

  (* devuelve la lista de accesos donde el llamante pone los argumentos *)
  val outAccs : frame -> access list

  (* devuelve la lista de accesos donde la función llamada ve los argumentos *)
  val inAccs : frame -> access list

  (* crea un acceso para una variable local *)
  val allocLocal : frame -> bool -> access

  (* crea un fragmento para un string *)
  val newStringFrag : temp.label -> string -> frag

  (* crea la expresión para acceder a una variable *)
  val accToExp : access -> tree.exp -> tree.exp

  (* invoca a una función externa *)
  val externalCall : string * tree.exp list -> tree.exp

  (* Agrega una instrucción ficticia al final de la lista de instrucciones para 
   * indicar que los registros callee-save estarán vivos a la salida de la función *)
  val procEntryExit2 : frame * assem.instr list -> assem.instr list

  (* Agrega prólogo y epílogo de una llamada a función una vez que se conoce
   * el tamaño del marco de activación de la función *)
  val procEntryExit3 : frame * assem.instr list ->
    {prolog: string, body: assem.instr list, epilog: string}

end
