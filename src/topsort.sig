(* Topological Sort *)
signature topsort = sig

  exception Ciclo

  (* crea un nuevo entorno de tipos a partir de otro y una lista de declaraciones de tipos *)
  val fijaTipos : {name: string, ty: ast.ty} list -> 
    (string, tentry.Tipo) table.Tabla -> 
    (string, tentry.Tipo) table.Tabla

end
