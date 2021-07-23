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

  type frame = {
      name: string,            (* nombre de la función *)
      formals: bool list,      (* cuales parámetros formales de la función son escapados *)
      locals: bool list,       (* cuales variables locales son escapadas *)
      actualLocal: int ref     (* ubicación de la última variable local en words *)
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
  
  val wSz = 8                  (* tamaño de la palabra *)
  val log2wSz = 3              (* log base 2 de word size *)

  val fpPrev = 0               (* ubicación del fp anterior *)
  val fpPrevLev = 2*wSz        (* ubicación del static link *)
  
  val localsInicial = 0        (* inicialmente no hay variables locales *)
  val localsGap = ~wSz         (* gap para el primer local en el stack *)
  
  val argsGap = 2*wSz          (* gap para el primer arg en el stack *)

  val argRegs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
  val callerSave = ["rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"]
  val calleeSave = ["rbx", "rbp", "rsp", "r12", "r13", "r14", "r15"]
  val specialRegs = [rv, fp, sp]

  (* crea un nuevo frame *)
  fun newFrame {name, formals} = {
      name = name,
      formals = formals,
      locals = [],
      actualLocal = ref 0
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
   * el segundo argumento indica si la variable está escapada o no
   *)
  fun allocLocal (f: frame) true =
        let
          val actualLocal = #actualLocal(f)
          val ret = InFrame (!actualLocal*wSz+localsGap)
          val _ = actualLocal := (!actualLocal - 1)
        in
          ret
        end
    | allocLocal (f: frame) false = InReg (temp.newTemp())
  
  (* TODO *)
  val allocArg = allocLocal

  (* newStringFrag : temp.label -> string -> frag
   * crea un fragmento para un string 
   *)
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
      val len = "  .quad " ^ makestring (stringLen s)
      val str = "  .ascii \""^s^"\""
      val value = label^":\n"^len^"\n"^str^"\n"
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

  (* TODO *)
  fun procEntryExit1 (frame, body) = body

end
