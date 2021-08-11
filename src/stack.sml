structure stack :> stack = struct

  type 'a Pila = 'a list ref

  fun nuevaPila() = ref []

  fun nuevaPila1 item = ref [item]

  fun nuevaPilaList items = ref items

  fun pushPila pila item = 
        pila := (item::(!pila))

  fun popPila pila = 
        pila := tl (!pila)

  fun topPila pila = 
        hd (!pila)

  fun retrievePila pila = 
    let
      val v = topPila pila
      val _ = popPila pila
    in
      v
    end

  fun pilaToList pila = !pila

  fun fold f i pila =
    let
      val l = !pila
      val _ = pila := []
    in
      foldl f i (!pila)
    end 

end
