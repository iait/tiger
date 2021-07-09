open lexer
open parser
open line
open astpp
open escape
open seman
open BasicIO Nonstdio
open ast
open error

fun lexstream (is: instream) =
  Lexing.createLexer (fn b => fn n => buff_input is b 0 n)

fun errParsing lbuf =
  error ("Parsing ["^(Lexing.getLexeme lbuf)^"]", !nlin)

fun parse src = 
  let
    val entrada = (open_in src) handle _ => raise Fail (src^" no existe!")
    val lexbuf = lexstream entrada
  in
    prog Tok lexbuf handle _ => errParsing lexbuf
  end

fun compile src () =
  let
    val _ = nlin := 1
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
      handle Fail s => (print (s^"\n"); false)
           | _ => (print "Error inesperado\n"; false))
  | expect f (SOME error) = ((f(); print "Ok\n"; false) 
      handle Fail s => (print (s^"\n"); isSuffix error s)
           | _ => (print "Error inesperado\n"; false))

val summary = ref [] : bool list ref

fun printResult result = (
  summary := result::(!summary); 
  print ("  Test: "^(if result then "Ok" else "Error")^"\n")
)

(* test01
 * Define un tipo array de int y una variable de ese tipo.
 *)
val _ = print "Test01\n";
val result = expect (compile "test/test01.tig") NONE
val _ = printResult result

(* test02
 * Igual que el anterior excepto que define un tipo sinónimo de int.
 *)
val _ = print "Test02\n";
val result = expect (compile "test/test02.tig") NONE
val _ = printResult result

(* test03
 * Define un tipo record {name: string, age: int} y una 
 * variable de ese tipo. Luego le asigna otro valor al campo name.
 *)
val _ = print "Test03\n";
val result = expect (compile "test/test03.tig") NONE
val _ = printResult result

(* test04
 * Define una variable de tipo int escapada por una función
 * definida a continuación.
 *)
val _ = print "Test04\n";
val result = expect (compile "test/test04.tig") NONE
val _ = printResult result

(* test05
 * Define variable que no debe ser escapada.
 *)
val _ = print "Test05\n";
val result = expect (compile "test/test05.tig") NONE
val _ = printResult result

(* test06
 * Define procedimientos mutuamente recursivos.
 *)
val _ = print "Test06\n";
val result = expect (compile "test/test06.tig") NONE
val _ = printResult result

(* test07
 * Define procedimientos mutuamente recursivos.
 *)
val _ = print "Test07\n";
val result = expect (compile "test/test07.tig") NONE
val _ = printResult result

(* test08
 * Expresión if-then-else correcta.
 *)
val _ = print "Test08\n";
val result = expect (compile "test/test08.tig") NONE
val _ = printResult result

(* test09
 * Expresión if-then-else incorrecta. 
 *)
val _ = print "Test09\n";
val result = expect (compile "test/test09.tig") 
  (SOME "los tipos del then y else no coinciden")
val _ = printResult result

(* test10
 * Expresión while incorrecta. 
 *)
val _ = print "Test10\n";
val result = expect (compile "test/test10.tig")
  (SOME "el cuerpo del while no puede devolver valor")
val _ = printResult result

(* test11 
 * Expresión del hi de un for de tipo string.
 *)
val _ = print "Test11\n";
val result = expect (compile "test/test11.tig") 
  (SOME "error en el tipo de hi")
val _ = printResult result

(* test12
 * Incrementa 100 veces una variable inicializada en 0 con un for.
 *)
val _ = print "Test12\n";
val result = expect (compile "test/test12.tig") NONE
val _ = printResult result

(* test13
 * Comparación de int con string. 
 *)
val _ = print "Test13\n";
val result = expect (compile "test/test13.tig") 
  (SOME "error de tipos")
val _ = printResult result

(* test14
 * Comparación de array con record.
 *)
val _ = print "Test14\n";
val result = expect (compile "test/test14.tig") 
  (SOME "tipos no comparables por desigualdad")
val _ = printResult result

(* test15
 * If-then sin else que devuelve un valor entero en then. 
 *)
val _ = print "Test15\n";
val result = expect (compile "test/test15.tig") 
  (SOME "el cuerpo del then no puede devolver valor")
val _ = printResult result

(* test16
 * Definiciones de tipo mutuamente recursivos con ciclos.
 *)
val _ = print "Test16\n";
val result = expect (compile "test/test16.tig")
  (SOME "ciclo en la declaración de tipos")
val _ = printResult result

(* test17
 * Define un tipo árbol pero entre tree y treelist hay una declaración
 * de variable que rompe con la definición de tipos mutuamente recursivos.
 *)
val _ = print "Test17\n";
val result = expect (compile "test/test17.tig") 
  (SOME "no existe el tipo \"treelist\"")
val _ = printResult result

(* test18
 * Define dos funciones mutuamente recursivas do_nothing1 y do_nothing2
 * pero en el medio hay una declaración de variable interrumpiendo. 
 *)
val _ = print "Test18\n";
val result = expect (compile "test/test18.tig") 
  (SOME "no existe la función \"do_nothing2\"")
val _ = printResult result

(* test19
 * Uso de variable no declarada en cuerpo de función. 
 *)
val _ = print "Test19\n";
val result = expect (compile "test/test19.tig") 
  (SOME "variable inexistente \"a\"")
val _ = printResult result

(* test20
 * Uso de variable no declarada en el cuerpo de un while. 
 *)
val _ = print "Test20\n";
val result = expect (compile "test/test20.tig") 
  (SOME "variable inexistente \"i\"")
val _ = printResult result

(* test21
 * Procedimiento que retorna un valor entero. 
 *)
val _ = print "Test21\n";
val result = expect (compile "test/test21.tig") 
  (SOME "cuerpo de la función \"fun\" incorrecto")
val _ = printResult result

(* test22
 * Intenta acceder a un campo inexistente de un record. 
 *)
val _ = print "Test22\n";
val result = expect (compile "test/test22.tig") 
  (SOME "no existe el campo \"nam\" en el record")
val _ = printResult result

(* test23
 * Asigna un int a un campo de record de tipo string. 
 *)
val _ = print "Test23\n";
val result = expect (compile "test/test23.tig") 
  (SOME "error de tipos en asignación")
val _ = printResult result

(* test24
 * Indexa una variable que no es de tipo array sino int. 
 *)
val _ = print "Test24\n";
val result = expect (compile "test/test24.tig") 
  (SOME "se esperaba un tipo array")
val _ = printResult result

(* test25
 * Intenta acceder a un campo de una variable que no es record sino int
 *)
val _ = print "Test25\n";
val result = expect (compile "test/test25.tig") 
  (SOME "se esperaba un tipo record para buscar el campo \"f\"")
val _ = printResult result

(* test26
 * Suma de int con string 
 *)
val _ = print "Test26\n";
val result = expect (compile "test/test26.tig") 
  (SOME "error de tipos")
val _ = printResult result

(* test27
 * Variable local oculta otra variable 
 *)
val _ = print "Test27\n";
val result = expect (compile "test/test27.tig") NONE
val _ = printResult result

(* test28
 * Declara dos tipos record rectype1 y rectype2 idénticos
 * Como para tiger son tipos distintos, al asignar un valor de un tipo a una 
 * variable del otro se produce un error. 
 *)
val _ = print "Test28\n";
val result = expect (compile "test/test28.tig") 
  (SOME "tipo \"rectype1\" no compatible con inicialización")
val _ = printResult result

(* test29
 * Declara dos tipos array arrtype1 y arrtype2 idénticos
 * Como para tiger son tipos distintos, al asignar un valor de un tipo a una 
 * variable del otro se produce un error.
 *)
val _ = print "Test29\n";
val result = expect (compile "test/test29.tig") 
  (SOME "tipo \"arrtype1\" no compatible con inicialización")
val _ = printResult result

(* test30
 * Declara un sinónimo de tipo. 
 *)
val _ = print "Test30\n";
val result = expect (compile "test/test30.tig") NONE
val _ = printResult result

(* test31
 * Inicializa con un string una variable de tipo int 
 *)
val _ = print "Test31\n";
val result = expect (compile "test/test31.tig") 
  (SOME "tipo \"int\" no compatible con inicialización")
val _ = printResult result

(* test32
 * Inicialización de array de int con valores string. 
 *)
val _ = print "Test32\n";
val result = expect (compile "test/test32.tig") 
  (SOME "tipo de init incorrecto")
val _ = printResult result

(* test33
 * Declara variable de tipo record inexistente. 
 *)
val _ = print "Test33\n";
val result = expect (compile "test/test33.tig") 
  (SOME "tipo inexistente \"rectype\"")
val _ = printResult result

(* test34
 * Invocación de función con tipos incorrectos. 
 *)
val _ = print "Test34\n";
val result = expect (compile "test/test34.tig") 
  (SOME "tipo incorrecto en arg 1 de la llamada a \"g\"")
val _ = printResult result

(* test35
 * Invocación de función con menos argumentos de los declarados. 
 *)
val _ = print "Test35\n";
val result = expect (compile "test/test35.tig") 
  (SOME "faltan argumentos en la llamada a \"g\"")
val _ = printResult result

(* test36
 * Invocación de función con más argumentos de los declarados.
 *)
val _ = print "Test36\n";
val result = expect (compile "test/test36.tig") 
  (SOME "sobran argumentos en la llamada a \"g\"")
val _ = printResult result

(* test37
 * Redeclaración de variables con el mismo nombre. Ok. 
 *)
val _ = print "Test37\n";
val result = expect (compile "test/test37.tig") NONE
val _ = printResult result

(* test38
 * Declara el mismo tipo dos veces en el mismo batch. 
 *)
val _ = print "Test38\n";
val result = expect (compile "test/test38.tig") 
  (SOME "declaración de tipo \"a\" duplicada")
val _ = printResult result

(* test39
 * Declara la misma función dos veces en el mismo batch. 
 *)
val _ = print "Test39\n";
val result = expect (compile "test/test39.tig")
  (SOME "declaración de tipo \"g\" duplicada")
val _ = printResult result

(* test40
 * Otro procedimiento que retorna valor. 
 *)
val _ = print "Test40\n";
val result = expect (compile "test/test40.tig") 
  (SOME "cuerpo de la función \"g\" incorrecto")
val _ = printResult result

(* test41
 * Asignación en la variable índice de un for.
 * Como es de solo lectora lanza error.  
 *)
val _ = print "Test41\n";
val result = expect (compile "test/test41.tig") 
  (SOME "asignación de variable índice de for \"i\"")
val _ = printResult result

(* test42
 * Varias declaraciones de tipos y variables junto con algunas
 * asignaciones. 
 *)
val _ = print "Test42\n";
val result = expect (compile "test/test42.tig") NONE
val _ = printResult result

(* test43
 * Inicialización con unit. 
 *)
val _ = print "Test43\n";
val result = expect (compile "test/test43.tig") 
  (SOME "inicialización incorrecta de variable \"a\"")
val _ = printResult result

(* test44
 * Inicialización y asignación de nil a un record. 
 *)
val _ = print "Test44\n";
val result = expect (compile "test/test44.tig") NONE
val _ = printResult result

(* test45
 * Inicialización con nil de una variable sin indicar su tipo. 
 *)
val _ = print "Test45\n";
val result = expect (compile "test/test45.tig") 
  (SOME "inicialización incorrecta de variable \"a\"")
val _ = printResult result

(* test46
 * Comparación de record con nil. 
 *)
val _ = print "Test46\n";
val result = expect (compile "test/test46.tig") NONE
val _ = printResult result

(* test47
 * Redeclaración de un tipo en otro batch. 
 *)
val _ = print "Test47\n";
val result = expect (compile "test/test47.tig") NONE
val _ = printResult result

(* test48
 * Redeclaración de una función en otro batch. 
 *)
val _ = print "Test48\n";
val result = expect (compile "test/test48.tig") NONE
val _ = printResult result

(* test49
 * Declaración y llamada de funciones mutuamente recursivas. 
 *)
val _ = print "Test49\n";
val result = expect (compile "test/test49.tig")  NONE
val _ = printResult result

(* test50
 * Break dentro de while. 
 *)
val _ = print "Test50\n";
val result = expect (compile "test/test50.tig") NONE
val _ = printResult result

(* test51
 * Break fuera de un loop. 
 *)
val _ = print "Test51\n";
val result = expect (compile "test/test51.tig") NONE (* TODO break *)
val _ = printResult result

(* test52
 * Definición de una función recursiva 
 *)
val _ = print "Test52\n";
val result = expect (compile "test/test52.tig") NONE
val _ = printResult result

(* test53
 * El scope de variables y de funciones es el mismo por lo que se produce error, 
 *)
val _ = print "Test53\n";
val result = expect (compile "test/test53.tig") 
  (SOME "se esperaba que \"a\" fuese una variable")
val _ = printResult result

(* test54
 * Define tipo local que oculta uno externo con el mismo nombre. 
 *)
val _ = print "Test54\n";
val result = expect (compile "test/test54.tig") NONE
val _ = printResult result

(* test55
 * Merge. 
 *)
val _ = print "Test55\n";
val result = expect (compile "test/test55.tig") NONE
val _ = printResult result

(* test56
 * Comparación nil=nil es incorrecta. 
 *)
val _ = print "Test56\n";
val result = expect (compile "test/test56.tig") 
  (SOME "tipos no comparables por igualdad")
val _ = printResult result

(* test57
 * Queens. 
 *)
val _ = print "Test57\n";
val result = expect (compile "test/test57.tig") NONE
val _ = printResult result

(* test58
 * Comentarios anidados. 
 *)
val _ = print "Test58\n";
val result = expect (compile "test/test58.tig") NONE
val _ = printResult result

(* test59
 * Define tipos recursivos válidos: intlist y tree. 
 *)
val _ = print "Test59\n";
val result = expect (compile "test/test59.tig") NONE
val _ = printResult result

(* test60
 * Prueba scopes de tipos, valores (variables y funciones) y campos de record. 
 *)
val _ = print "Test60\n";
val result = expect (compile "test/test60.tig") NONE
val _ = printResult result

(* test61
 * Error de sintaxis: rectype nil 
 *)
val _ = print "Test61\n";
val result = expect (compile "test/test61.tig") 
 (SOME "Parsing [nil]")
val _ = printResult result

(* test62
 * Record con campos repetidos produce error. 
 *)
val _ = print "Test62\n";
val result = expect (compile "test/test62.tig") 
  (SOME "el record \"rectype\" tiene el campo duplicado \"name\"")
val _ = printResult result

(* test63
 * Función con parámetros repetidos produce error. 
 *)
val _ = print "Test63\n";
val result = expect (compile "test/test63.tig") 
  (SOME "la función \"f\" tiene el parámetro duplicado \"name\"")
val _ = printResult result

(* test64
 * Si el tipo existe en el batch se usa ese, sino se usa el tipo
 * definido en algún batch anterior. 
 *)
val _ = print "Test64\n";
val result = expect (compile "test/test64.tig") NONE
val _ = printResult result

local
  val l = !summary
  val correctos = Int.toString (List.foldl (fn (b, i) => if b then i+1 else i) 0 l)
  val total = Int.toString (length l)
in
  val _ = print ("Total de test correctos: "^correctos^"/"^total^"\n")
end
