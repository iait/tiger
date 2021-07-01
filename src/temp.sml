structure temp :> temp = struct

  type label = string
  type temp = string

  local
    val i = ref 0
    val j = ref 0
  in
    fun newTemp() =
      let
        val s = "T"^Int.toString(!i)
      in
        i := !i+1; s
      end

    fun newLabel() =
      let
        val s = "L"^Int.toString(!j)
      in
        j := !j+1; s
      end
  end
  
  fun namedLabel s = s

end
