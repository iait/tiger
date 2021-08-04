open lexer
open parser
open line
open astpp
open escape
open seman
open BasicIO Nonstdio
open error
open trans
open interp
open codegen
open assem

(* lexstream : instream -> lexbuf *)
fun lexstream (is: instream) =
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
    val (code, l5)    = arg (l4, "-code")
    val (flow, l6)    = arg (l5, "-flow")
    val (inter, l7)   = arg (l6, "-inter")
    (* instream *)
    val entrada = case l7 
      of [n] => ((open_in n) handle _ => raise Fail (n^" no existe!"))
       | []  => std_in
       | _   => raise Fail "opción desconocida!"
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
    (*val DEGUB = print 
      ("cantidad de fragmentos: "^((Int.toString o List.length) fragList)^"\n")*)
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
    val _ = if inter then interp true ps ss else ()
    (* genera assembler de los procedimientos *)
    val _ = List.app (codegen (flow, inter)) ps
  in
    print "----Fin de la compilación\n"
  end	handle Fail s => print("Fail: "^s^"\n")

val _ = main (CommandLine.arguments())
