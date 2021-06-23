structure stack :> stack = struct

  type 'a Pila = 'a list ref

  fun nuevaPila() = ref []

  fun nuevaPila1 item = ref [item]

  fun pushPila pila item = 
        pila := (item::(!pila))

  fun popPila pila = 
        pila := tl (!pila)

  fun topPila pila = 
        hd (!pila)

end
