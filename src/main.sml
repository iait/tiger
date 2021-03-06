open lexer
open parser
open line
open astpp
open escape
open seman
open Nonstdio
open TextIO
open error
open trans
open interp
open codegen
open assem
open util
open temp
open Process
open frame
open regalloc

(* lexstream : instream -> lexbuf *)
fun lexstream (is: BasicIO.instream) =
  Lexing.createLexer (fn b => fn n => buff_input is b 0 n);

(* errParsing : lexbuf -> 'a *)
fun errParsing lbuf =
  error ("Parsing ["^(Lexing.getLexeme lbuf)^"]", !nlin)

fun main args =
  let
    val _ = print "----Inicio de la compilación\n"
    (* arg : string list * string ->  bool * string list *)
    fun arg (l, s) = (List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
    val (arbol, l1)   = arg (args, "-arbol")
    val (escapes, l2) = arg (l1, "-escapes")
    val (ir, l3)      = arg (l2, "-ir")
    val (canon, l4)   = arg (l3, "-canon")
    val (inter, l5)   = arg (l4, "-inter")
    val (code, l6)    = arg (l5, "-code")
    val (alloc, l7)   = arg (l6, "-alloc")
    (* instream *)
    val (srcPath, entrada) = case l7
      of [n] => ((n, BasicIO.open_in n) handle _ => raise Fail (n^" no existe!"))
       | _   => raise Fail "opción desconocida!"
    val _ = if isSuffix ".tig" srcPath then () else raise Fail "debe tener extensión .tig"
    (* scanner *)
    val lexbuf = lexstream entrada
    (* parser *)
    val _ = print "----Análisis sintáctico\n"
    val expr : ast.exp = prog Tok lexbuf handle _ => errParsing lexbuf
    (* imprime el árbol antes del cálculo de escapes *)
    val _ = if arbol then printAst expr else ()
    (* calcula variables escapadas *)
    val _ = print "----Cálculo de escapes\n"
    val _ = findEscape expr
    (* imprime el árbol luego del cálculo de escapes *)
    val _ = if escapes then printAst expr else ()
    (* chequeo de tipos y traducción a código intermedio *)
    val _ = print "----Traducción a código intermedio\n"
    val _ = transProg expr
    val fragList : frag list = getResult()
    (* imprime código intermedio *)
    val _ = if ir then printIr fragList else ()
    (* canoniza código intermedio *)
    val _ = print "----Canonización\n"
    val canonList : frag list = canonize fragList
    (* imprime código intermedio canonizado *)
    val _ = if canon then printIr canonList else ()
    (* separa lista de fragmentos en procedimientos y strings *)
    val (ps: (tree.stm list * frame.frame) list, ss: (label * string) list) = 
      splitFrags canonList
    (* interpreta código intermedio canonizado *)
    val _ = if inter then interpret true ps ss else ()
    (* crea archivo para el assembler *)
    val execPath = (substring (srcPath, 0, (size srcPath)-(size ".tig")))
    val assemPath = execPath^".s"
    val out = openOut assemPath
    val progName = List.last (String.fields (fn x => x = #"/") execPath)
    val _ = output (out, "\n# programa "^progName^"\n\n")
    val _ = output (out, "  .file \""^progName^".tig\"\n\n")
    (* genera assembler para los strings *)
    val _ = if ss=[] then () else 
      (output (out, "  .section .rodata\n");
      List.app (fn (l,s) => output (out, l^":\n"^s)) ss;
      output (out, "\n"))
    (* función que genera assembler para un procedimiento *)
    fun assem (stms, frame) =
      let
        val instrs = codegen stms
        (* imprime las instrucciones antes de la asignación de registros *)
        val _ = if not code then () else
          (print "Instrucciones originales\n";
          print ((name frame)^":\n");
          List.app (print o (format id)) instrs)
        (* calcula la asignación de registros *)
        val (is, saytemp) = regalloc alloc frame (procEntryExit2 (frame, instrs))
        (* genera prólogo y epílogo de la función *)
        val {prolog, body, epilog} = procEntryExit3 (frame, is)
      in
        output (out, prolog);
        List.app (fn instr => output (out, format saytemp instr)) body;
        output (out, epilog)
      end
    (* genera assembler para todos los procedimientos *)
    val _ =
      (output (out, "  .section .text\n");
      List.app assem ps;
      output (out, "\n"))
    (* cierra el archivo *)
    val _ = closeOut out
    (* enlaza con el runtime y genera el ejecutable *)
    val status = system 
      ("gcc -x assembler "^assemPath^" -x none -lruntime -Lsrc -no-pie -o "^execPath)
    val _ = if status=success then () else raise Fail "Fallo al enlazar con runtime"
  in
    print "----Fin de la compilación\n"
  end	handle Fail s => print("Fail: "^s^"\n")

val _ = main (CommandLine.arguments())
