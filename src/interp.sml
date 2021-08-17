structure interp :> interp = struct

  open table
  open Dynarray
  open tree
  open frame
  open temp
  open treepp

  (* Memoria y registros *)
  local
    val tabTemps: (temp, int ref) Tabla ref = ref (tabNueva())
    val tabMem: (int, int ref) Tabla ref = ref (tabNueva())

    fun load tab a =
      case tabBusca(a, !tab) of
        SOME v => !v
        | NONE => (tab := tabInserta(a, ref 0, !tab); 0)
    fun store tab a x =
      case tabBusca(a, !tab) of
        SOME v => v := x
        | NONE => tab := tabInserta(a, ref x, !tab)
  in
    val loadMem = load tabMem
    val storeMem = store tabMem
    fun printMem () =
      let
        val ls = tabAList(!tabMem)
        fun p (a,b) = (print(Int.toString(a)); print(" -> "); print(Int.toString(!b)); print("\n"))
      in
        (print("MEM:\n"); List.app p ls)
      end

    val loadTemp = load tabTemps
    val storeTemp = store tabTemps
    fun getTemps () = 
      let
        val tabL = tabAList(!tabTemps)
      in
        map (fn (x,y) => (x, !y)) tabL
      end
    fun restoreTemps temps = map (fn (x,y) => storeTemp x y) temps
    fun printTemps () =
      let
        val ls = tabAList(!tabTemps)
        fun p (a,b) = (print(a); print(" -> "); print(Int.toString(!b)); print("\n"))
      in
        (print("TEMPS:\n"); List.app p ls)
      end
  end
  
  (* alocación de memoria *)
  local
    val nextfree = ref 0
  in
    fun getNewMem(n) =
    let
      val r = !nextfree
    in
      (nextfree := !nextfree + (n*wSz); r)
    end
  end
  
  (* tabla de labels -> direcciones *)
  local
    val tabLabels: (label, int) Tabla ref = ref (tabNueva())
  in
    fun loadLabel lab = case tabBusca(lab, !tabLabels) of
      SOME a => a
      | NONE => raise Fail("Label no encontrado: "^lab^"\n")
    fun storeLabel lab addr = tabLabels := tabInserta(lab, addr, !tabLabels)
  end
  
  (* Guardado de strings *)
  local
    val stringArray = array(10, "")
    val next = ref 0;
  in
    fun loadString addr = sub(stringArray, loadMem addr)
    fun storeString str =
      let
        val addr = getNewMem(1)
        val idx = !next;
        val _ = next := !next + 1;
      in
        (update(stringArray, idx, str); storeMem addr idx; addr)
      end
  end

  (*
    Intérprete de código intermedio canonizado.

    inter showDebug funFracs stringFracs recibe:
    showDebug: bool 
      false sólo ejecuta el código, 
      true muestra en cada paso la instrucción a ejecutar y el estado de la memoria y temporarios
    funFracs: (stm list * frame) list
      Cada elemento de la lista es un par con la lista de stms devuelta por el canonizador
      y el frame de la función
    stringFracs: (label * string) list
      Lista con un elemento por cada string definido en el código.
      Cada elemento es un par formado por el label y el string.

    inter usa: 
      Constantes de frame: wSz, rv, fp
      Funciones de frame: formals, exp. formals (Appel, pág 135) debe devolver una lista con los
      frame.access de cada argumento pasado a la función, esto es el frame.access que se usa en el
      body de la función para referirse a cada argumento. También debe devolver un frame.access 
      para el static link, como primer elemento de la lista.

    Nota: en una máquina de N bits los enteros de ML tienen N-1 bits.
    El intérprete fallará si se usan números muy grandes.
  *)

  (* inter : bool -> (stm list * frame) list -> (label * string) list -> unit *)
  fun interpret showDebug (funFracs: (stm list * frame) list) (stringFracs: (label * string) list) =
  let

    val _ = List.map (fn (lab, str) => storeLabel lab (storeString str)) stringFracs

    (* Funciones de biblioteca *)
    fun initArray(siz::init::rest) =
          let
            val mem = getNewMem(siz)
            val l = (mem+1, siz)::(List.tabulate(siz, (fn x => (mem+wSz*x, init))))
            val _ = List.map (fn (a,v) => storeMem a v) l
          in
            mem
          end
      | initArray _ = raise Fail("No debería pasar (initArray)")

    fun checkIndexArray(arr::idx::rest) =
          let
            val siz = loadMem (arr+1)
            val _ = if (idx>=siz orelse idx<0) then raise Fail("Índice fuera de rango\n") else ()
          in
            0
          end
      | checkIndexArray _ = raise Fail("No debería pasar (checkIndexArray)")
    
    fun allocRecord(ctos::vals) =
          let
            val mem = getNewMem(ctos)
            val addrs = List.tabulate(ctos, (fn x => mem + x*wSz))
            val l = ListPair.zip(addrs, vals)
            val _ = List.map (fn (a,v) => storeMem a v) l
          in
            mem
          end
      | allocRecord _ = raise Fail("No debería pasar (allocRecord)")
    
    fun checkNil(r::rest) =
          let
            val _ = if (r=0) then raise Fail("Nil\n") else ()
          in
            0
          end
      | checkNil _ = raise Fail("No debería pasar (checkNil)")

    fun stringCompare(strPtr1::strPtr2::rest) =
          let
            val str1 = loadString strPtr1
            val str2 = loadString strPtr2
            val res = String.compare(str1, str2)
          in
            case res of
              LESS => ~1
              | EQUAL => 0
              | GREATER => 1
          end
      | stringCompare _ = raise Fail("No debería pasar (stringCompare)")

    fun printFun(strPtr::rest) =
          let
            val str = loadString strPtr
            val _ = print(str)
          in
            0
          end
      | printFun _ = raise Fail("No debería pasar (printFun)")

    fun flushFun(args) = 0

    fun ordFun(strPtr::rest) =
          let
            val str = loadString strPtr
            val ch = hd(explode(str))
          in
            ord(ch)
          end
      | ordFun _ = raise Fail("No debería pasar (ordFun)")

    fun chrFun(i::rest) =
          let
            val ch = chr(i)
            val str = implode([ch])
          in
            storeString str
          end
      | chrFun _ = raise Fail("No debería pasar (chrFun)")

    fun sizeFun(strPtr::rest) =
          let
            val str = loadString strPtr
          in
            String.size(str)
          end
      | sizeFun _ = raise Fail("No debería pasar (sizeFun)")

    fun substringFun(strPtr::first::n::rest) =
          let
            val str = loadString strPtr
            val substr = String.substring(str, first, n)
          in
            storeString substr
          end
      | substringFun _ = raise Fail("No debería pasar (substringFun)")

    fun concatFun(strPtr1::strPtr2::rest) =
          let
            val str1 = loadString strPtr1
            val str2 = loadString strPtr2
            val res = str1^str2
          in
            storeString res
          end
      | concatFun _ = raise Fail("No debería pasar (concatFun)")

    fun notFun(v::rest) =
          if (v=0) then 1 else 0
      | notFun _ = raise Fail("No debería pasar (notFun)")

    fun getstrFun(args) = 
      let
        val str = TextIO.inputLine TextIO.stdIn
      in
        storeString str
      end

    val tabLib: (label, int list -> int) Tabla =
      tabInserList(tabNueva(),
        [("_initArray", initArray),
        ("_checkIndexArray", checkIndexArray),
        ("_allocRecord", allocRecord),
        ("_checkNil", checkNil),
        ("_stringcmp", stringCompare),
        ("print", printFun),
        ("flush", flushFun),
        ("ord", ordFun),
        ("chr", chrFun),
        ("size", sizeFun),
        ("substring", substringFun),
        ("concat", concatFun),
        ("not", notFun),
        ("getstr", getstrFun)])

    (* Evalúa una expresión, devuelve el valor (entero) *)
    fun evalExp(CONST t) = t
    | evalExp(NAME n) = loadLabel n
    | evalExp(TEMP t) = loadTemp t
    | evalExp(BINOP(b, e1, e2)) =
      let
        val ee1 = evalExp(e1)
        val ee2 = evalExp(e2)
      in
        case b of
          PLUS => ee1+ee2
          | MINUS => ee1-ee2
          | MUL => ee1*ee2
          | DIV => ee1 div ee2
          | AND => Word.toInt(Word.andb(Word.fromInt(ee1), Word.fromInt(ee2)))
          | OR => Word.toInt(Word.orb(Word.fromInt(ee1), Word.fromInt(ee2)))
          | LSHIFT => Word.toInt(Word.<<(Word.fromInt(ee1), Word.fromInt(ee2)))
          | RSHIFT => Word.toInt(Word.>>(Word.fromInt(ee1), Word.fromInt(ee2)))
          | ARSHIFT => Word.toInt(Word.~>>(Word.fromInt(ee1), Word.fromInt(ee2)))
          | XOR => Word.toInt(Word.xorb(Word.fromInt(ee1), Word.fromInt(ee2)))
      end
    | evalExp(MEM(e)) =
      let
        val ee = evalExp(e)
      in
        loadMem ee
      end
    | evalExp(CALL(f, args)) =
      let
        val lab = case f of
          NAME l => l
          | _ => raise Fail("CALL a otra cosa (no implemetado)\n")
        val eargs = List.map evalExp args
        (*Si lab es de biblioteca, usar la función de la tabla*)
        val rv = case tabBusca(lab, tabLib) of
          SOME f => f(eargs)
          | NONE => evalFun(lab, eargs)
      in
        (storeTemp frame.rv rv; rv)
      end
    | evalExp(ESEQ(s, e)) = raise Fail("No canonizado\n")
    (* ejecuta un comando, devuelve NONE si no salta, SOME l si salta al label l *)
    and evalStm(MOVE(TEMP t, e)) = (storeTemp t (evalExp(e)); NONE)
    | evalStm(MOVE(MEM(e1), e2)) = (storeMem (evalExp(e1)) (evalExp(e2)); NONE)
    | evalStm(MOVE(_, _)) = raise Fail("MOVE a otra cosa\n")
    | evalStm(EXP e) = (evalExp(e); NONE)
    | evalStm(JUMP(e, ls)) =
      let
        val lab = case e of
          NAME l => l
          | _ => raise Fail("JUMP a otra cosa\n")
      in
        SOME lab
      end
    | evalStm(CJUMP(rop, e1, e2, lt, lf)) =
      let
        val ee1 = evalExp(e1)
        val ee2 = evalExp(e2)
        val b = case rop of
          EQ => ee1=ee2
          | NE => ee1<>ee2
          | LT => ee1<ee2
          | GT => ee1>ee2
          | LE => ee1<=ee2
          | GE => ee1>=ee2
          | ULT => Word.fromInt(ee1)<Word.fromInt(ee2)
          | UGT => Word.fromInt(ee1)>Word.fromInt(ee2)
          | ULE => Word.fromInt(ee1)<=Word.fromInt(ee2)
          | UGE => Word.fromInt(ee1)>=Word.fromInt(ee2)
      in
        if (b) then SOME lt else SOME lf
    end
    | evalStm(SEQ(_,_)) = raise Fail("No canonizado\n")
    | evalStm(LABEL _) = NONE
    (* Ejecuta una llamada a función *)
    and evalFun(f, args) =
      let
        (* Encontrar la función*)
        val ffrac = List.filter (fn (body, frame) => frame.name(frame)=f) funFracs
        val _ = 
          if ((List.length ffrac) = 1) then () 
          else raise Fail ("No se encuentra la función, o repetida: "^f^"\n")
        val (body, frame) = List.hd ffrac
        (* Mostrar qué se está haciendo, si showDebug *)
        val _ = 
          if not showDebug then () 
          else (
            print((frame.name frame)^":\n");
            List.app (print o printTreeStm) body;
            print("Argumentos: ");
            List.app (fn n => (print(Int.toString(n));
            print("  "))) args;
            print("\n"))

        fun execute l =
        let
          fun exe [] = ()
          | exe (x::xs) =
            let
              val _ = 
                if not showDebug then ()
                else (
                  printTemps();
                  printMem();
                  print("****************\n");
                  print(printTreeStm(x));
                  print("****************\n"))
            in
              case evalStm x of
                SOME lab =>
                  let
                    fun f [] = raise Fail("No está el label en la función\n")
                    | f (x::xs) =
                      (case x of
                        LABEL y => if (y=lab) then (x::xs) else f xs
                        | _ => f xs)
                  in
                    exe (f l)
                  end
                | NONE => exe xs
            end
        in
          exe l
        end

        (* Guardar temporarios *)
        val temps = getTemps()
        (* Mover fp lo suficiente *)
        val fpPrev = loadTemp frame.fp
        val _ = storeTemp frame.fp (fpPrev-1024*1024)
        (* Poner argumentos donde la función los espera *)
        val formals = map (fn x => frame.accToExp x (TEMP frame.fp)) (inAccs frame)
        val formalsValues = ListPair.zip(formals, args)
        val _ = map (fn (x,y) => 
          case x of
            TEMP t => storeTemp t y
            | MEM m => storeMem (evalExp m) y
            | _ => raise Fail "Formals inesperado!") formalsValues
        (* Ejecutar la lista de instrucciones *)
        val _ = execute body
        val rv = loadTemp frame.rv
        (* Restaurar temporarios *)
        val _ = restoreTemps temps
        val _ = storeTemp frame.rv rv
      in
        rv
      end
  in 
    (print("Comienzo de ejecución...\n");
    evalFun("_tigermain", []);
    print("Fin de ejecución.\n"))
  end

end
