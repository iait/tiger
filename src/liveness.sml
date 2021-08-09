structure liveness :> liveness = struct

  open table
  open Splayset
  open util
  open temp
  open flow
  open graph
  open assem

  type move = temp.temp * temp.temp

  (* grafo de interferencias *)
  type interGraph = {
    adj: (temp, temp set) Tabla,
    mov: (temp, temp set) Tabla
  }

  (* Crea una copia del grafo de interferencias *)
  fun fromInterGraph {adj, mov} = {adj = fromTab adj, mov = fromTab mov}

  (* agrega una arista *)
  fun addEdge adj (t1, t2) =
    if t1 = t2 then ()
    else
      let
        val adj1 = (tabSaca (t1, adj))
        val adj2 = (tabSaca (t2, adj))
      in
        tabMete (t1, add (adj1, t2), adj);
        tabMete (t2, add (adj2, t1), adj)
      end

  (* elimina una arista *)
  fun removeEdge adj (t1, t2) =
    let
      val adj1 = tabSaca (t1, adj)
      val adj2 = tabSaca (t2, adj)
    in
      tabMete (t1, delete (adj1, t2), adj);
      tabMete (t2, delete (adj2, t1), adj)
    end

  (* agrega un nodo *)
  fun addNode adj t = case tabBusca (t, adj) of
    NONE => tabMete (t, makeTempSet [], adj)
    | SOME _ => raise Fail "el nodo ya existía!"

  (* elimina un nodo *)
  fun removeNode adj t = 
    if not (isEmpty (tabSaca (t, adj))) then
      raise Fail "el nodo tiene aristas!"
    else
      (tabElimina (t, adj); ())

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
  fun flow2interGraph {control,def,use,isMove,nodes,temps} =
    let
      (* función para calcular el liveIn y liveOut de cada temporal *)
      fun liveness ns (lms as {liveIn,liveOut}) =
        let
          (* crea una copia de los mapas *)
          val lms' : liveMaps = {
            liveIn = fromTab liveIn, liveOut = fromTab liveOut
          }
          (* une una lista de conjuntos *)
          fun unionList [] = makeTempSet []
            | unionList [s] = s
            | unionList (s1::s2::ss) = unionList (union (s1, s2)::ss)
          (* itera sobre los nodos *)
          fun iter [] lms = ()
            | iter (n::ns) (lms as {liveIn,liveOut}) =
                let
                  (*val _ = print ("nodo: "^(Int.toString n)^"\n")*)
                  fun tabSacaOrEmpty (n, t) = 
                    tabSaca (n, t) handle noExiste => makeTempSet []
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
      val {liveIn, liveOut} = liveness ns {liveIn=tabNueva(), liveOut=tabNueva()}
      (* crea el grafo de interferencias *)
      val (inter as {adj, mov}) = {adj=tabNueva(), mov=tabNueva()}
      (* crea todos los nodos *)
      val ts = tabClaves temps
      val _ = List.app (fn t => tabMete (t, makeTempSet[], adj)) ts
      val _ = List.app (fn t => tabMete (t, makeTempSet[], mov)) ts
      (* función para calcular el grafo de interferencias *)
      (* interference : (int * instr) * move list -> move list *)
      fun interference ((n, OPER _), ms) =
            let
              val defs = listItems (tabSaca (n, def))
              val outs = listItems (tabSaca (n, liveOut))
              val _ = List.app (fn a => List.app (fn b => addEdge adj (a, b)) outs) defs
            in
              ms
            end
        | interference ((n, MOV {assem,dst,src}), ms) =
            let
              val outs = listItems (tabSaca (n, liveOut))
              val _ = addEdge mov (src, dst)
              val _ = List.app (fn b => if b=src then () else addEdge adj (dst, b)) outs
            in
              (src,dst)::ms
            end
        | interference ((n, LAB _), ms) = raise Fail "No debería llegar LAB"
      val ms = List.foldl interference [] (tabAList nodes)
    in
      (inter, ms)
    end

  (* imprime el grafo de interferencias para debug *)
  fun showInterGraph ({adj,mov} : interGraph) =
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
      val _ = (print "mov:\n"; showTabla (2, showTemp, showTempSet, mov))
    in () end

end
