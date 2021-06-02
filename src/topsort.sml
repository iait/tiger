structure topsort :> topsort = struct
  
  open ast
  open table
  open typ
  
  exception Ciclo
  
  (* arma grafo a partir de las declaraciones de tipos *)
  fun genGraph batch =
    let
      fun aux [] graph = graph
        | aux ({name, ty=NameTy s}::lt) (ns, es) = aux lt (name::ns, (s, name)::es)
        | aux ({name, ty=ArrayTy s}::lt) (ns, es) = aux lt (name::ns, (s, name)::es)
        | aux ({name, ty=RecordTy fl}::lt) (ns, es) = aux lt (name::ns, es)
    in
      aux batch ([], [])
    end
  
  (* retorna true si el elemnto es miembro de la lista *)
  fun mem (_, []) = false
    | mem (x, h::t) = x=h orelse mem (x, t)
  
  (* busca los nodos salientes *)
  fun nexts (_, []) = []
    | nexts (a, (x, y)::pairs) =
        if a=x then y::(nexts (a, pairs))
        else nexts (a, pairs)
  
  (* devuelve un orden topológico de los nodos *)
  fun topsort nodes graph =
      let
        fun sort ([], path, visited) = visited
          | sort (x::xs, path, visited) = 
              if mem (x, path) then raise Ciclo else
              let
                val visited' = if mem (x, visited)  then visited
                               else x::(sort (nexts (x, graph), x::path, visited))
              in
                sort (xs, path, visited')
              end
      in
        sort (nodes, [], [])
      end

  (* procesa las declaraciones de tipos en el orden especificado creando un nuevo entorno *)
  fun process oldenv newenv tydecs order =
    let
      (* busca la declaración de tipo del símbolo y lo procesa *)
      fun procSymbol name = procTy name (tabSaca (name, tydecs)) 
            handle noExiste => raise Fail ("no debería pasar!")

      (* inserta el tipo procesado en el nuevo entorno *)
      and procTy name (NameTy s) = (case tabBusca (s, newenv) of
            SOME t => tabMete (name, t, newenv)
            | NONE => (case tabBusca (s, oldenv) of
                SOME t => tabMete (name, t, newenv)
                | NONE => raise Fail ("no existe el tipo \""^s^"\"")))
        | procTy name (ArrayTy s) = (case tabBusca (s, newenv) of
            SOME t => tabMete (name, TArray (t, ref()), newenv)
            | NONE => (case tabBusca (s, oldenv) of
                SOME t => tabMete (name, TArray (t, ref()), newenv)
                | NONE => raise Fail ("no existe el tipo \""^s^"\"")))
        | procTy name (RecordTy fl) = tabMete (name, TRecord (List.map 
            (fn {name,escape,typ} => (name, ref (TTipo typ))) fl, ref()), newenv)

      (* procesa las declaraciones de tipos en orden y llena el newenv *)
      val _ = List.app procSymbol order

      (* crea un nuevo entorno con los tipos anteriores y los nuevos *)
      val env = tabInserList (oldenv, tabAList newenv)

      (* completa las referencias de los fields de un tipo record *)
      fun fillRef (TRecord ([], _)) = ()
        | fillRef (TRecord ((_, r)::fl, u)) = (case !r of 
            TTipo s => (case tabBusca (s, env) of 
                SOME t => (r:=t; fillRef (TRecord (fl, u)))
                | NONE => raise Fail ("no existe el tipo \""^s^"\""))
            | _ => fillRef (TRecord (fl, u)))
        | fillRef _ = ()

      (* segunda pasada para completar los records *)
      val _ = List.app fillRef (tabValores newenv)
    in
      env
    end

  (* completa records *)
  fun replaceRefs env = ()

  (* crea un nuevo entorno con las declaraciones de tipos del batch *)
  fun fijaTipos batch env =
    let
      val (nodes, edges) = genGraph batch
      val order = topsort nodes edges
      val tydecs = tabInserList (tabNueva(), List.map (fn {name,ty} => (name,ty)) batch)
      val env' = process env (tabNueva()) tydecs order
    in
      env'
    end

end
