(*
    Frames para el x86_64:
    Los 6 primeros argumentos en registros: rdi, rsi, rdx, rcx, r8, r9
    Resto de los argumentos van en el stack

        |    argn     |    fp+8*(n-4)
        |    ...      |
        |    arg8     |    fp+32
        |    arg7     |    fp+24
        | static link |    fp+16 (fp del nivel estático anterior del llamado)
        |  retorno    |    fp+8
        |   fp ant    |    fp
        ---------------
        |   local1    |    fp-8
        |   local2    |    fp-16
        |    ...      |
        |   localn    |    fp-8*n

*)

structure frame :> frame = struct

  open tree
  open assem

  type frame = {
      name: string,            (* nombre de la función *)
      formals: bool list,      (* cuales parámetros formales de la función son escapados *)
      locals: bool list,       (* cuales variables locales son escapadas *)
      localCount: int ref      (* número de variables locales *)
  }
  
  type register = string

  (* acceso a una variable o parámetro formal de función *)
  datatype access = InFrame of int (* offset desde el fp *)
                  | InReg of temp.temp

  (* fragmento de assembler TODO revisar este comentario
   * puede ser procedimiento con un árbol de código intermedio
   * o un string con etiqueta.
   *)
  datatype frag = PROC of {body: tree.stm list, frame: frame} 
                | STRING of temp.label * string

  val rv = "rax"               (* return value  *)
  val fp = "rbp"               (* frame pointer *)
  val sp = "rsp"               (* stack pointer *)
  val rax = "rax"              (* para la división *)
  val rdx = "rdx"              (* para la división *)
  
  val wSz = 8                  (* tamaño de la palabra *)
  val log2wSz = 3              (* log base 2 de word size *)

  val fpPrev = 0               (* ubicación del fp anterior *)
  val fpPrevLev = 2*wSz        (* ubicación del static link *)
  
  val argsGap = 2*wSz          (* gap para el primer arg en el stack *)

  val argRegs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
  val callerSave = ["rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"]
  val calleeSave = ["rbx", "r12", "r13", "r14", "r15"]
  val machineRegs = callerSave @ calleeSave (* 14 registros *)

  (* crea un nuevo frame *)
  fun newFrame {name, formals} = {
      name = name,
      formals = formals,
      locals = [],
      localCount = ref 0
  }

  (* obtiene el nombre del frame *)
  fun name (f: frame) = #name f

  (* formals : frame -> access list
   * crea una lista de accesos para los argumentos de una función
   * envía los primeros 6 argumentos a registros y el resto en el stack
   *)
  fun formals (f: frame) =
    let 
      fun aux [] _ _ acs = acs
            | aux (true::t) regs n acs = aux t regs (n+1) (InFrame (n*wSz+argsGap) :: acs)
            | aux (false::t) [] n acs = aux t [] (n+1) (InFrame (n*wSz+argsGap) :: acs)
            | aux (false::t) (r::regs) n acs = aux t regs n (InReg r :: acs)
    in
      aux (#formals(f)) argRegs 0 []
    end

  (* allocLocal : frame -> bool -> access
   * crea un acceso para una variable local
   * el segundo argumento indica si la variable está escapada o no *)
  fun allocLocal (f: frame) true =
        let
          val localCount = #localCount(f)
          val _ = localCount := (!localCount + 1)
          val ret = InFrame (!localCount * ~wSz)
        in
          ret
        end
    | allocLocal (f: frame) false = InReg (temp.newTemp())
  
  (* TODO *)
  val allocArg = allocLocal

  (* newStringFrag : temp.label -> string -> frag
   * crea un fragmento para un string *)
  fun newStringFrag label s = 
    let
      fun stringLen s =
        let
          fun aux [] = 0
            | aux (t1::t2::t3::t4::t) = 
                if t1=(#"\\") andalso t2=(#"x") then 1 + (aux t)
                else 1 + (aux (t2::t3::t4::t))
            | aux (_::t) = 1 + (aux t)
        in
          aux (explode s)
        end
      val len = "  .quad "^(makestring (stringLen s))^"\n"
      val str = "  .ascii \""^s^"\"\n"
      val value = len^str
    in
      STRING (label, value)
    end

  (* exp : frame.access -> tree.exp -> tree.exp
   * recibe el acceso a una variable, la expresión para acceder al fp
   * donde pertenece la variable y crea expresión para acceder a la variable
   *)
  fun exp (InFrame k) e = MEM (BINOP (PLUS, e, CONST k))
    | exp (InReg t) _ = TEMP t

  (* externalCall : string * tree.exp list -> tree.exp
   * invoca a una función externa 
   *)
  fun externalCall (s, l) = CALL (NAME s, l)

  (* Agrega una instrucción ficticia al final de la lista de instrucciones para 
   * indicar que los registros callee-save estarán vivos a la salida de la función *)
  (* procEntryExit2 : frame * instr list -> instr list *)
  fun procEntryExit2 (frame, body) =
    body @ [OPER {assem="", dst=[], src=(rv::calleeSave), jmp=[]}]

  (* Agrega prólogo y epílogo de una función una vez que se conoce el tamaño del 
   * marco de activación de la misma *)
  (* procEntryExit3 : frame * instr list -> {prolog: string, body: instr list, epilog: string} *)
  fun procEntryExit3 (frame, body) =
    let
      val name = name frame
      val size = !(#localCount(frame)) * wSz
      val allocStack = 
        if size=0 then "" 
        else "  subq $"^(Int.toString size)^", %rsp\n"
      val prolog = 
        "  .globl "^name^"\n"^
        "  .type "^name^", @function\n"^
        name^":\n"^
        "  pushq %rbp\n"^
        "  movq %rsp, %rbp\n"^
        allocStack
      val epilog = 
        "  movq %rbp, %rsp\n"^
        "  popq %rbp\n"^
        "  ret\n"^
        "  .size "^name^", .-"^name^"\n\n"
    in
      {prolog=prolog, body=body, epilog=epilog}
    end

end
