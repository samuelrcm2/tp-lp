(* Plc interpreter main file *)

fun run (exp: expr) : string =
  let
    val exptype = teval exp [];
    val expval = eval exp []
  in
    (val2string expval) ^ " : " ^ (type2string exptype)
  end;