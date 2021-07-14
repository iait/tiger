signature trans = sig

  exception Break

  type level
  type access
  type frag = frame.frag

  (* level de _tigermain *)
  val outermost : level
  
  (* crea un nuevo level a partir del level padre *)
  val newLevel : 
    {parent: level, name: temp.label, formals: bool list} -> level
    
  (* crea lista de accesos para los parámetros de una función *)
  val formals : level -> access list
  
  (* obtiene el level actual *)
  val getActualLev : unit -> int
  
  (* TODO *)
  val allocArg : level -> bool -> access
  
  (* crea un acceso para una variable local *)
  val allocLocal : level -> bool -> access

  type exp
  
  val procEntryExit : {level: level, body: exp} -> unit
  
  (***** Traducción de expresiones *****)
  
  (* variables *)
  val simpleVar : access * int -> exp
  val fieldVar : exp * int -> exp
  val subscriptVar : exp * exp -> exp
  
  (* constantes *)
  val unitExp : unit -> exp
  val nilExp : unit -> exp
  val intExp : int -> exp
  val stringExp : string -> exp
  
  (* llamada a función *)
  val callExp : 
    {name: temp.label, extern: bool, proc: bool, lev: level, args: exp list} -> exp
  
  (* operaciones binarias *)
  val binOpIntExp : {left:exp, oper:ast.oper, right:exp} -> exp
  val binOpIntRelExp: {left:exp, oper:ast.oper, right:exp} -> exp
  val binOpStrExp : {left:exp, oper:ast.oper, right:exp} -> exp
  val nilCompare : {record:exp, oper:ast.oper} -> exp
  
  (* inicialización de record y array *)
  val recordExp : (exp * int) list -> exp
  val arrayExp : {size: exp, init: exp} -> exp
  
  (* condicionales *)
  val ifThenExp : {test: exp, then': exp} -> exp
  val ifThenElseExp : {test: exp, then': exp, else': exp} -> exp
  val ifThenElseExpUnit : {test: exp, then': exp, else': exp} -> exp
  
  (* ciclos *)
  val preLoopExp : unit -> unit
  val breakExp : unit -> exp
  val postLoopExp : unit -> unit
  val whileExp : {test: exp, body: exp} -> exp
  val forExp : {lo: exp, hi: exp, var: exp, body: exp} -> exp

  (* declaraciones let *)
  val letExp : exp list * exp -> exp
  
  (* secuencia *)
  val seqExp : exp list -> exp
  
  (* asignación *)
  val assignExp : {var: exp, exp: exp} -> exp
  
  (* declaración de variable *)
  val varDec : access -> exp
  
  (* declaración de función *)
  val preFunctionDec : unit -> unit
  val functionDec : exp * level * bool -> unit
  val postFunctionDec : unit -> unit

  (* intermediate representation *)
  val getResult : unit -> frag list
  val printIr : frag list -> unit

  (* canonización *)
  val canonize : frag list -> frag list
  
  (* separa lista de fragmentos en procedimientos y strings *)
  val splitFrags : 
    frag list -> (tree.stm list * frame.frame) list * (temp.label * string) list

end
