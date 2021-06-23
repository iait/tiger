(*
    Frames para el 80386 (sin displays ni registers).

        |    argn    |    fp+4*(n+1)
        |    ...     |
        |    arg2    |    fp+16
        |    arg1    |    fp+12
        |  fp level  |    fp+8
        |  retorno   |    fp+4
        |   fp ant   |    fp
        --------------
        |   local1   |    fp-4
        |   local2   |    fp-8
        |    ...     |
        |   localn   |    fp-4*n
*)

structure frame :> frame = struct

  open tree

  type frame = {
      name: string,
      formals: bool list,
      locals: bool list,
      actualLocal: int ref
  }

  type register = string

  datatype access = InFrame of int 
                  | InReg of temp.temp
                  | todo

  datatype frag = PROC of {body: tree.stm, frame: frame}
                | STRING of temp.label * string

  val rv = "RV"                (* return value  *)
  val ov = "OV"                (* overflow value (edx en el 386) *)
  val fp = "FP"                (* frame pointer *)
  val sp = "SP"                (* stack pointer *)

  val fpPrev = 0               (* offset (bytes) *)
  val fpPrevLev = 8            (* offset (bytes) *)
  val wSz = 4                  (* word size in bytes *)
  val log2WSz = 2              (* base two logarithm of word size in bytes *)
  val callDefs = [rv]
  val callerSaves = []
  val calleeSaves = []

  val localsInicial = 0            (* words *)
  val localsGap = ~4               (* bytes *)
  val specialRegs = [rv, fp, sp]
  val argRegs = []

  fun newFrame {name, formals} = {
      name = name,
      formals = formals,
      locals = [],
      actualLocal = ref localsInicial
  }

  fun name (f: frame) = #name f

  fun formals (f: frame) = [] (* COMPLETAR *)

  fun allocLocal (f: frame) b =
      case b of
        true =>
          let
              val ret = InFrame (!(#actualLocal f) * wSz + localsGap)
              val _ = #actualLocal f := (!(#actualLocal f) - 1)
          in ret end
      | false => InReg (temp.newTemp())

  val allocArg = allocLocal

  fun string (l, s) = l^(temp.makeString s)^"\n"

  fun exp (InFrame k) e = MEM (BINOP (PLUS, TEMP fp, CONST k))
    | exp (InReg l) e = TEMP l
    | exp todo e = raise Fail "TODO!"

  fun externalCall (s, l) = CALL (NAME s, l)

  fun procEntryExit1 (frame, body) = body

end
