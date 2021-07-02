signature translate = sig

  exception Break
  exception DivByZero

  type level
  type access
  val TODO : access
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
  val SCAF : exp
  
  val procEntryExit : {level: level, body: exp} -> unit
  val getResult : unit -> frag list
  val unitExp : unit -> exp
  val nilExp : unit -> exp
  val intExp : int -> exp
  val stringExp : string -> exp
  val simpleVar : access * int -> exp
  val varDec : access -> exp
  val fieldVar : exp * int -> exp
  val subscriptVar : exp * exp -> exp
  val recordExp : (exp * int) list -> exp
  val callExp : temp.label * bool * bool * level * exp list -> exp
  val letExp : tree.stm list * exp -> exp
  val breakExp : unit -> exp
  val seqExp : exp list -> exp
  val preWhileForExp : unit -> unit
  val postWhileForExp : unit -> unit
  val whileExp : {test: exp, body: exp, lev:level} -> exp
  val forExp : {lo: exp, hi: exp, var: exp, body: exp} -> exp
  val ifThenExp : {test: exp, then': exp} -> exp
  val ifThenElseExp : {test: exp, then': exp, else': exp} -> exp
  val ifThenElseExpUnit : {test: exp, then': exp, else': exp} -> exp
  val assignExp : {var: exp, exp:exp}-> exp
  val preFunctionDec : unit -> unit
  val functionDec : exp * level * bool -> exp
  val postFunctionDec : unit -> unit
  val binOpIntExp : {left:exp, oper:ast.oper, right:exp} -> exp
  val binOpIntRelExp: {left:exp, oper:ast.oper, right:exp} -> exp
  val binOpStrExp : {left:exp, oper:ast.oper, right:exp} -> exp
  val arrayExp : {size: exp, init: exp} -> exp

  val Ir : frag list -> string

end
