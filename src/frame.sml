(*
    Frames para el x86_64:
    Los 6 primeros argumentos en registros: rdi, rsi, rdx, rcx, r8, r9
    Resto de los argumentos van en el stack

    |    argn     |    fp + (n-5)*wSz
    |    ...      |
    |    arg8     |    fp + 3*wSz
    |    arg7     |    fp + 2*wSz
    |  ret addr   |    fp + 1*wSz
    |   fp ant    |    fp
    ---------------
    |   local1    |    fp - 1*wSz (static link)
    |   local2    |    fp - 2*wSz
    |    ...      |
    |   localn    |    fp - n*wSz

*)

structure frame :> frame = struct

  open tree
  open assem
  open temp

  (* acceso a una variable o parámetro formal de función *)
  datatype access = InFrame of int (* offset desde el fp *)
                  | InReg of temp.temp

  type frame = {
      name: string,          (* nombre de la función *)
      outAccs: access list,  (* donde el llamante pone los argumentos *)
      inAccs: access list,   (* donde la función llamada ve los argumentos *)
      locCount: int ref      (* número de variables locales *)
  }

  (* fragmento del programa:
   * puede ser procedimiento con un frame y una lista de sentencias de código intermedio
   * o un string con etiqueta. *)
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
  val fpPrevLev = ~wSz        (* ubicación del static link *)
  
  val argsGap = 2*wSz          (* gap para el primer arg en el stack *)

  val argRegs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
  val callerSave = ["rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"]
  val calleeSave = ["rbx", "r12", "r13", "r14", "r15"]
  val machineRegs = callerSave @ calleeSave (* 14 registros *)
  val specialRegs = ["rbp", "rsp"]

  (* crea un nuevo frame *)
  (* newFrame : {name: string, formals: bool list} -> frame *)
  fun newFrame {name, formals} =
    let
      (* crea accesos donde el llamante pone los argumentos *)
      fun createOutAccs 0 regs argCount = []
        | createOutAccs n [] argCount =
            (InFrame (argCount*wSz + 2*wSz)) :: (createOutAccs (n-1) [] (argCount+1))
        | createOutAccs n (r::rs) argCount =
            (InReg r) :: (createOutAccs (n-1) rs argCount)

      (* crea accesos donde la función llamada verá los argumentos *)
      val locCount = ref 0
      fun createInAccs [] = []
        | createInAccs (true::fs) =
            (locCount := (!locCount + 1);
            (InFrame (!locCount * ~wSz)) :: (createInAccs fs))
        | createInAccs (false::fs) =
            (InReg (newTemp())) :: (createInAccs fs)
    in
      {
        name = name,
        outAccs = createOutAccs (length formals) argRegs 0,
        inAccs = createInAccs formals,
        locCount = locCount
      }
    end

  (* obtiene el nombre del frame *)
  (* name : frame -> string *)
  fun name {name,outAccs,inAccs,locCount} = name

  (* devuelve la lista de accesos donde el llamante pone los argumentos *)
  fun outAccs {name,outAccs,inAccs,locCount} = outAccs

  (* devuelve la lista de accesos donde la función llamada ve los argumentos *)
  fun inAccs {name,outAccs,inAccs,locCount} = inAccs

  (* crea un acceso para una variable local *)
  (* allocLocal : frame -> bool -> access *)
  fun allocLocal {name,outAccs,inAccs,locCount} false = InReg (newTemp())
    | allocLocal {name,outAccs,inAccs,locCount} true =
      let
        val _ = locCount := (!locCount + 1) 
      in
        InFrame (!locCount * ~wSz)
      end

  (* crea un fragmento para un string *)
  (* newStringFrag : temp.label -> string -> frag *)
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

  (* recibe el acceso a una variable, la expresión para acceder al fp
   * donde pertenece la variable y crea expresión para acceder a la variable *)
  (* accToExp : frame.access -> tree.exp -> tree.exp *)
  fun accToExp (InFrame k) fpExp = MEM (BINOP (PLUS, fpExp, CONST k))
    | accToExp (InReg t) fpExp = TEMP t

  (* invoca a una función externa *)
  (* externalCall : string * tree.exp list -> tree.exp *)
  fun externalCall (s, l) = CALL (NAME s, l)

  (* Agrega una instrucción ficticia al final de la lista de instrucciones para 
   * indicar que los registros callee-save estarán vivos a la salida de la función *)
  (* procEntryExit2 : frame * instr list -> instr list *)
  fun procEntryExit2 (frame, body) =
    body @ [OPER {assem="", dst=[], src=(rv::calleeSave), jmp=[]}]

  (* Agrega prólogo y epílogo de una función una vez que se conoce el tamaño del 
   * marco de activación de la misma *)
  (* procEntryExit3 : frame * instr list -> {prolog: string, body: instr list, epilog: string} *)
  fun procEntryExit3 ({name,outAccs,inAccs,locCount}, body) =
    let
      val size = !locCount * wSz
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
