signature table = sig

  type ('a, 'b) Tabla

  exception yaExiste of string
  exception noExiste
  exception noExisteS of string

  (* Crea una nueva tabla *)
  val tabNueva : unit -> (''a, 'b) Tabla

  (* Crea una copia de la tabla *)
  val fromTab : (''a, 'b) Tabla -> (''a, 'b) Tabla

  (* Función identidad *)
  val name : 'a -> 'a

  (* Indica si la clave está usada o no en la tabla *)
  val tabEsta : ''a * (''a, 'b) Tabla -> bool

  (* Crea una nueva tabla y le asocia el nuevo par sólo si no existía ya la clave *)
  val tabInserta : ''a * 'b * (''a, 'b) Tabla -> (''a, 'b) Tabla
  
  (* Mete la nueva asociación en la tabla *)
  val tabMete : ''a * 'b * (''a, 'b) Tabla -> unit

  (* Crea una nueva tabla y le asocia el nuevo par, reemplazando la asociación
   * anterior a la clave si existiese
   *)
  val tabRInserta : ''a * 'b * (''a, 'b) Tabla -> (''a, 'b) Tabla

  (* Busca el valor asociado a una clave *)
  val tabBusca : ''a * (''a, 'b) Tabla -> 'b option

  (* Busca el valor asociado a una clave, si la clave no está en la tabla
   * lanza excepción. A pesar de que la función se llama "saca" no altera
   * el contenido de la tabla!
   *)
  val tabSaca : ''a * (''a, 'b) Tabla -> 'b

  (* Elimina la asociación de la clave de la tabla *)
  val tabElimina : ''a * (''a, 'b) Tabla -> 'b

  (* Devuelve una nueva tabla cuyos valores son el resultado de aplicarle la 
   * función proporcionada 
   *)
  val tabAplica : ('a -> 'b) * (''c, 'a) Tabla -> (''c, 'b) Tabla

  (* Devuelve una nueva tabla cuyas claves y valores son el resultado de 
   * aplicarle las funciones proporcionadas
   *)
  val tabAAplica : ('a -> ''c) * ('b -> 'd) * ('a, 'b) Tabla -> (''c, 'd) Tabla

  (* Igual que la función anterior solo que inserta los pares en la nueva 
   * tabla en el orden inverso. Recordar que si una clave ya existe se
   * reemplaza su valor asociado.
   *)
  val tabRAAplica : ('a -> ''b) * ('c -> 'd) * ('a, 'c) Tabla -> (''b, 'd) Tabla

  (* Crea una nueva tabla copia de la anterior y le inserta todos los pares
   * de la lista proporcionada.
   *)
  val tabInserList : ('a, 'b) Tabla * ('a * 'b) list -> ('a, 'b) Tabla

  (* Lista los pares de la tabla *)
  val tabAList : ('a, 'b) Tabla -> ('a * 'b) list

  (* Devuelve una nueva tabla copia de la anterior sin los elementos filtrados
   * por la función proporcionada que aplica sobre los valores. *)
  val tabFiltra : ('b -> bool) * (''a, 'b) Tabla -> (''a, 'b) Tabla

  (* Devuelve el primer par de la tabla que cumpla el filtro *)
  val tabPrimer : ('b -> bool) * ('a, 'b) Tabla -> ('a * 'b)

  (* Retorna una lista con todas las claves *)
  val tabClaves : ('a, 'b) Tabla -> 'a list

  (* Retorna una lista con todos los valores *)
  val tabValores : ('a, 'b) Tabla -> 'b list

  (* Compara por igualdad dos tablas *)
  val tabIguales : ('b * 'b -> bool) -> ((''a, 'b) Tabla * (''a, 'b) Tabla) -> bool

  (* Imprime el contenido de la tabla para debug *)
  val showTabla : int * ('a -> string) * ('b -> string) * ('a, 'b) Tabla -> unit

end
