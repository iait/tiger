structure printtyp :> printtyp = struct

  open table
  open typ

  val uniqueList = ref [] : unit ref list ref
  
  fun uniqueToInt u [] n = (uniqueList := u::(!uniqueList); length (!uniqueList))
    | uniqueToInt u (x::xs) n = if u=x then n else uniqueToInt u xs (n-1)
  
  fun punique u = 
    let 
      val l = !uniqueList
      val n = uniqueToInt u l (length l)
    in
      print (Int.toString n)
    end
  
  fun indent 0 = ()
    | indent n = (print "\t"; indent (n-1))
  
  fun ptyp TUnit _ _ = print "TUnit"
    | ptyp TNil _ _ = print "TNil"
    | ptyp TInt _ _ = print "TInt"
    | ptyp TString _ _ = print "TString"
    | ptyp (TArray (t, u)) n ul = (print "TArray "; punique u; print " of "; ptyp t n ul)
    | ptyp (TRecord (fl, u)) n ul = (print "TRecord "; punique u;
        if List.exists (fn u' => u'=u) ul then ()
        else (print " of "; pfl fl (n+1) (u::ul)))
    | ptyp (TTipo _) _ _ = raise Fail ("tipo TTipo no soportado")
  and pfl [] n ul = print ""
    | pfl ((s, r)::xs) n ul = (print "\n"; indent n; print (s^": "); ptyp (!r) n ul; pfl xs n ul)
  
  fun printTyp t = ptyp t 0 []
  
  fun findPrintTyp tenv s = case tabBusca (s, tenv) of
    SOME t => (print (s^": "); printTyp t; print "\n")
    | NONE => raise Fail ("no se encuentra el tipo \""^s^"\"")
  
  fun printTEnv tenv = 
    List.app (findPrintTyp tenv) (tabClaves tenv)

end