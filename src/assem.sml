structure assem = struct

  type reg = string
  type temp = temp.temp
  type label = temp.label

  datatype instr = OPER of {assem: string,
                            dst: temp list,
                            src: temp list,
                            jmp: label list}
                 | LAB of {assem: string,
                             lab: label}
                 | MOV of {assem: string,
                           dst: temp,
                           src: temp}

  (* format : (temp -> string) -> instr -> string *)
  (* formatea una instrucción como su string en lenguaje assembler.
   * saytemp es una función que traduce un temp a su registro asignado.
   *)
  fun format saytemp instr =
    let
      (* saylabel traduce un label a su etiqueta final *)
      fun saylabel label = label
      (* charToInt convierte caracter a entero *)
      fun charToInt c = ord c - ord #"0"
      (* reemplaza en el string pseudo-assembler las referencias a temps y labels *)
      fun speak assem dst src jmp =
        let
          fun replace [] = []
            | replace [c1] = [c1]
            | replace [c1, c2] = [c1, c2]
            | replace (c1::c2::c3::rest) =
                if c1 = #"`" then
                  if c2 = #"s" then 
                    (explode (saytemp (List.nth (src, charToInt c3))) @ replace rest)
                  else if c2 = #"d" then
                    (explode (saytemp (List.nth (dst, charToInt c3))) @ replace rest)
                  else if c2 = #"j" then
                    (explode (saylabel (List.nth (jmp, charToInt c3))) @ replace rest)
                  else
                    raise Fail "Imposible, error al formatear assembler"
                else
                  c1 :: replace (c2::c3::rest) 
        in
          implode (replace (explode assem))
        end
    in
      case instr of
        OPER {assem, dst, src, jmp=jmp} => "  "^(speak assem dst src jmp)^"\n"
        | LAB {assem, lab} => assem^"\n"
        | MOV {assem, dst, src} => "  "^(speak assem [dst] [src] [])^"\n"
    end

end
