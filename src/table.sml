structure table :> table = struct

  open Polyhash
  open util

  type ('a, 'b) Tabla = ('a, 'b) hash_table

  exception yaExiste of string
  exception noExiste
  exception noExisteS of string
  
  fun tabNueva() = mkPolyTable(100, noExiste)

  fun fromTab t =
        let
          val t' = tabNueva()
        in
          apply (fn x => insert t' x) t; t'
        end

  fun name x = x

  fun tabEsta (s, t) = 
        case peek t s 
          of SOME _ => true
           | NONE   => false

  fun tabInserta (s, e, t) = let val t' = copy t in (peekInsert t' (s, e); t') end

  fun tabMete (s, e, t) = insert t (s, e)

  fun tabRInserta (s, e, t) = let val t' = copy t in (insert t' (s, e); t') end

  fun tabBusca(s, t) = peek t s

  fun tabSaca(s, t) =
        case tabBusca (s, t) 
          of SOME e => e
           | NONE => raise noExiste

  fun tabElimina(s, t) =
        case tabBusca (s, t) 
          of SOME e => remove t s
           | NONE => raise noExiste

  fun tabAplica (f, t) = map (fn (_, e) => f e) t

  fun tabAAplica (f, g, t) = 
        let
          val l' = listItems t
          val t' = tabNueva()
        in
          List.app (fn (k, e) => insert t' (k, e))
            (List.map (fn (k, e) => (f k, g e)) l');
          t'
        end

  fun tabRAAplica (f, g, t) = 
        let
          val l' = rev (listItems t)
          val t' = tabNueva()
        in
          List.app (fn (k, e) => insert t' (k, e))
            (List.map (fn (k, e) => (f k, g e)) l');
          t'
        end

  fun tabInserList (t, l) = 
        let
          val t' = copy t 
        in
          (List.app (fn (s, e) => insert t' (s, e)) l; t')
        end

  fun tabAList t = listItems t

  fun tabFiltra (f, t) =
        let
          val l = listItems t
          val t' = tabNueva()
        in
          List.app (fn (k, e) => insert t' (k,e))
            (List.filter (fn (_, b) => f b) l);
          t'
        end

  fun tabPrimer (f, t) = hd (List.filter (fn (_, b) => f b) (listItems t))

  fun tabClaves t = List.map (fn (x, _) => x) (listItems t)

  fun tabValores t = List.map (fn (_, x) => x) (listItems t)

  fun tabIguales cmp (t1, t2) =
    let
      val temp = fromTab t2
      fun aux [] = tabClaves temp = nil
        | aux ((k,v)::ts) = 
            (cmp (v, tabElimina (k, temp)) andalso aux ts) handle noExiste => false
    in
      aux (tabAList t1)
    end

  (* imprime el contenido de la tabla para debug *)
  fun showTabla (n, showKey, showValue, t) =
    let
      fun aux [] = ()
        | aux ((a, b)::tl) = 
            (print ((indent n)^(showKey a)^": "^(showValue b)^"\n"); aux tl)
    in aux (tabAList t) end

end
