open lexer
open parser
open line
open printast
open escape
open seman
open BasicIO Nonstdio
open ast

fun lexstream (is: instream) =
  Lexing.createLexer (fn b => fn n => buff_input is b 0 n);

fun errParsing lbuf = (
  print("Error en parsing!(" ^ (makestring (!nlin)) ^ ")[" ^ (Lexing.getLexeme lbuf) ^ "]\n");
  raise Fail "fin!"
)

fun parse src = 
  let
    val entrada = (open_in src) handle _ => raise Fail (src^" no existe!")
    val lexbuf = lexstream entrada
  in
    prog Tok lexbuf handle _ => errParsing lexbuf
  end

fun compile src () =
  let
    val _ = print "  Compilation: "
    val expr = parse src
  in
    transProg expr
  end

fun isSuffix s1 s2 = 
  let
    val revert = String.implode o rev o String.explode
  in
    String.isPrefix (revert s1) (revert s2)
  end

fun expect f NONE = ((f(); print "Ok\n"; true)
      handle _ => (print "Error inesperado\n"; false))
  | expect f (SOME error) = ((f(); false) 
      handle Fail s => (print (s^"\n"); isSuffix error s)
           | _ => (print "Error inesperado\n"; false))

fun printResult b = print ("  Test: "^(if b then "Ok" else "Error")^"\n")

(* test01 *)
val _ = print "Test01\n";
val result = expect (compile "test/test01.tig") NONE
val _ = printResult result

(* test02 *)
val _ = print "Test02\n";
val result = expect (compile "test/test02.tig") NONE
val _ = printResult result

(* test03 *)
val _ = print "Test03\n";
val result = expect (compile "test/test03.tig") NONE
val _ = printResult result

(* test04 *)
val _ = print "Test04\n";
val result = expect (compile "test/test04.tig") NONE
val _ = printResult result

(* test05 *)
val _ = print "Test05\n";
val result = expect (compile "test/test05.tig") NONE
val _ = printResult result

(* test06 *)
val _ = print "Test06\n";
val result = expect (compile "test/test06.tig") NONE
val _ = printResult result

(* test07 *)
val _ = print "Test07\n";
val result = expect (compile "test/test07.tig") NONE
val _ = printResult result

(* test08 *)
val _ = print "Test08\n";
val result = expect (compile "test/test08.tig") NONE
val _ = printResult result

(* test09 *)
val _ = print "Test09\n";
val result = expect (compile "test/test09.tig") 
  (SOME "los tipos del then y else no coinciden")
val _ = printResult result

(* test10 *)
val _ = print "Test10\n";
val result = expect (compile "test/test10.tig")
  (SOME "el cuerpo del while no puede devolver valor")
val _ = printResult result
