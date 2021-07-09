open lexer
open parser
open line
open astpp
open escape
open seman
open BasicIO Nonstdio
open error
open trans

(* lexstream : instream -> lexbuf *)
fun lexstream (is: instream) =
  Lexing.createLexer (fn b => fn n => buff_input is b 0 n);

(* errParsing : lexbuf -> 'a *)
fun errParsing lbuf =
  error ("Parsing ["^(Lexing.getLexeme lbuf)^"]", !nlin)

fun main args =
  let
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
    val lexbuf = lexstream entrada
    val expr = prog Tok lexbuf handle _ => errParsing lexbuf
    val _ = findEscape expr
    val _ = if arbol then printAst expr else ()
    val _ = transProg expr
    val _ = if ir then print (Ir (getResult())) else ()
  in
    print "yes!!\n"
  end	handle Fail s => print("Fail: "^s^"\n")

val _ = main (CommandLine.arguments())
