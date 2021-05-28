structure escape :> escape = struct

  open ast
  open table

  type depth = int
  type escEnv = (string, depth * bool ref) Tabla

  fun travVar env d var = 
      case var of
      SimpleVar s => (
          case tabBusca (s, env) of 
          SOME (dd, b) => if d > dd then b := true else ()
          | NONE => raise Fail ("escape "^s^" inexist"))
      | FieldVar (var', _) => travVar env d var'
      | SubscriptVar (var', exp) => (travVar env d var'; travExp env d exp)

  and travExp env d exp = 
      case exp of
      VarExp (var, _) => travVar env d var
      | CallExp ({func, args}, _) => List.app (travExp env d) args
      | OpExp ({left, oper, right}, _) => (travExp env d left; travExp env d right)
      | RecordExp ({fields, typ}, _) => List.app (fn (_, exp') => travExp env d exp') fields
      | SeqExp (le, _) => List.app (travExp env d) le
      | AssignExp ({var, exp}, _) => (travVar env d var; travExp env d exp)
      | IfExp ({test, then', else'=NONE}, _) => (travExp env d test; travExp env d then')
      | IfExp ({test, then', else'=SOME exp'}, _) => 
          (travExp env d test; travExp env d then'; travExp env d exp')
      | WhileExp ({test, body}, _) => (travExp env d test; travExp env d body)
      | ForExp ({var, escape, lo, hi, body}, _) =>
          let
            val env' = tabRInserta (var, (d, escape), env);
          in
            travExp env d lo;
            travExp env d hi;
            travExp env' d body
          end
      | LetExp ({decs, body}, _) => travExp (travDecs env d decs) d body
      | ArrayExp ({typ, size, init}, _) => (travExp env d size; travExp env d init)
      | _ => ()

  and travDecs env d [] = env
    | travDecs env d (s::t) =
        let
          fun travDec (FunctionDec []) = env
            | travDec (FunctionDec (({name, params, result, body}, _)::fs)) =
                let
                  val l = List.map (fn {name, escape, typ} => (name, (d+1, escape))) params
                  val env' = tabInserList (env, l)
                in
                  travExp env' (d+1) body; travDec (FunctionDec fs)
                end
            | travDec (VarDec ({name, escape, typ, init}, _)) =
                (travExp env d init; tabRInserta (name, (d, escape), env))
            | travDec (TypeDec _) = env
          val env' = travDec s
        in  travDecs env' d t end

  fun findEscape prog = travExp (tabNueva()) 0 prog
end
