structure liveness :> liveness = struct

  open table
  open Splayset
  open util
  open temp
  open flow
  open graph
  open assem

  (* grafo de interferencias *)
  type interGraph = {
    adj: (temp, temp set) Tabla,
    moves: (temp * temp) list
  }

  (* crea un conjunto nuevo de temporales *)
  fun makeSet() = empty String.compare

  (* agrega una arista al grafo de interferencias *)
  fun addEdge adj (t1, t2) =
    let
      val adj1 = case tabBusca (t1, adj) of
        NONE => makeSet()
        | SOME s => s
      val _ = tabMete (t1, add (adj1, t2), adj)
      val adj2 = case tabBusca (t2, adj) of
        NONE => makeSet()
        | SOME s => s
      val _ = tabMete (t2, add (adj2, t1), adj)
    in () end

  (* mapas con los liveIn y liveOut de cada nodo *)
  type liveMaps = {
    liveIn: (node, temp set) Tabla,
    liveOut: (node, temp set) Tabla
  }
  
  (* compara dos pares de liveIn y liveOut *)
  fun liveEqual lms lms' =
    let
      val {liveIn,liveOut} = lms
      val {liveIn=liveIn', liveOut=liveOut'} = lms'
    in
      tabIguales equal (liveIn, liveIn') andalso tabIguales equal (liveOut, liveOut')
    end

  (* calcula el grafo de interferencias a partir del control-flow graph *)
  fun flow2interGraph {control,def,use,isMove,nodes} =
    let
      (* función para calcular el liveIn y liveOut de cada temporal *)
      fun liveness ns (lms as {liveIn,liveOut}) =
        let
          (* crea una copia de los mapas *)
          val lms' : liveMaps = {
            liveIn = fromTab liveIn, liveOut = fromTab liveOut
          }
          (* une una lista de conjuntos *)
          fun unionList [] = makeSet()
            | unionList [s] = s
            | unionList (s1::s2::ss) = unionList (union (s1, s2)::ss)
          (* itera sobre los nodos *)
          fun iter [] lms = ()
            | iter (n::ns) (lms as {liveIn,liveOut}) =
                let
                  val _ = print ("nodo: "^(Int.toString n)^"\n")
                  fun tabSacaOrEmpty (n, t) = 
                    tabSaca (n, t) handle noExiste => makeSet()
                  (* calcula nuevo liveIn para n *)
                  val use = tabSaca (n, use)
                  val def = tabSaca (n, def)
                  val out = tabSacaOrEmpty (n, liveOut) 
                  val in' = union (use, difference (out, def))
                  val _ = tabMete (n, in', liveIn)
                  (* calcula nuevo liveOut para n *)
                  val succ = listItems (succ control n)
                  val out' = unionList (List.map (fn n' => tabSacaOrEmpty (n', liveIn)) succ)
                  val _ = tabMete (n, out', liveOut)
                in iter ns lms end
          val _ = iter ns lms'
        in
          if liveEqual lms lms' then lms else liveness ns lms'
        end
      (* calcula el liveIn y liveOut de cada temporal *)
      val ns = rev (tabClaves nodes)
      (*val _ = (print (showStrList (List.map Int.toString ns)); print "\n")*)
      val {liveIn, liveOut} = liveness ns {liveIn=tabNueva(), liveOut=tabNueva()}
      (* función para calcular el grafo de interferencias *)
      fun interference [] liveOut inter = inter
        | interference ((n, OPER _)::ns) liveOut (inter as {adj,moves}) =
            let
              val defs = listItems (tabSaca (n, def))
              val outs = listItems (tabSaca (n, liveOut))
              val _ = List.app (fn a => List.app (fn b => addEdge adj (a, b)) outs) defs
            in
              interference ns liveOut inter
            end
        | interference ((n, MOV {assem,dst,src})::ns) liveOut {adj,moves} =
            let
              val outs = listItems (tabSaca (n, liveOut))
              val _ = List.app (fn b => if b=src then () else addEdge adj (dst, b)) outs
            in
              interference ns liveOut {adj=adj, moves=(src,dst)::moves}
            end
        | interference ((n, LAB _)::ns) liveOut inter = raise Fail "No debería llegar LAB"
    in
      interference (tabAList nodes) liveOut {adj=tabNueva(), moves=[]}
    end

  (* imprime el grafo de interferencias para debug *)
  fun showInterGraph ({adj,moves} : interGraph) =
    let
      fun showTemp t = t
      fun showTempSet s =
        let
          fun aux [] = ""
            | aux [t] = t
            | aux (t::ts) = t^", "^(aux ts)
        in "{"^(aux (listItems s))^"}" end
      fun showMove (t1, t2) = t2^"<-"^t1
      val _ = (print "adj:\n"; showTabla (2, showTemp, showTempSet, adj))
      val _ = (print "moves:\n"; print (showStrList (List.map showMove moves)); print "\n")
    in () end

end