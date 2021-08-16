signature assem = sig

  datatype instr = OPER of {assem: string,
                            dst: temp.temp list,
                            src: temp.temp list,
                            jmp: temp.label list}
                 | LAB of {assem: string,
                             lab: temp.label}
                 | MOV of {assem: string,
                           dst: temp.temp,
                           src: temp.temp}

    (* Para debug *)
    val showInstr : instr -> string

    (* Formatea una instrucción a su string en lenguaje assembler.
     * Recibe una función que traduce un temp a su registro asignado. *)
    val format : (temp.temp -> string) -> instr -> string

end

