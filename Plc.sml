(* Plc interpreter main file *)

fun run (exp: expr) : string =
  let
    val exptype = teval exp [];
    val expval = eval exp []
  in
    (val2string expval) ^ " : " ^ (type2string exptype)
  end
  handle
    SymbolNotFound => print "\n*** PLC Environment Error: Symbol not found. ***\n"
  | EmptySeq => print "\n*** PLC Type Error: Empty input sequence. ***\n"
  | UnknownType => print "\n*** PLC Type Error: Unknown type. ***\n"
  | NotEqTypes => print "\n*** PLC Type Error: Type mismatch in comparison. ***\n"
  | WrongRetType => print "\n*** PLC Type Error: Declared return type does not match function body. ***\n"
  | DiffBrTypes => print "\n*** PLC Type Error: Type mismatch on If branches bodies. ***\n"
  | IfCondNotBool => print "\n*** PLC Type Error: Type mismatch on If condition; Condition must be Bool. ***\n"
  | NoMatchResults => print "\n*** PLC Type Error: No Match results. ***\n"
  | MatchResTypeDiff => print "\n*** PLC Type Error: Type mismatch on Match options' bodies. ***\n"
  | MatchCondTypesDiff => print "\n*** PLC Type Error: Type mismatch on Match options' conditions. ***\n"
  | CallTypeMisM => print "\n*** PLC Type Error: Type mismatch on function call. ***\n"
  | NotFunc => print "\n*** PLC Type Error: Not a function. ***\n"
  | ListOutOfRange => print "\n*** PLC Type Error: Attempting to access an item out of range. ***\n"
  | OpNonList => print "\n*** PLC Type Error: Attempting to access an item but expression is not a list. ***\n"
  | Impossible => print "\n*** PLC Interpreter Error: Impossible operation. ***\n"
  | HDEmptySeq => print "\n*** PLC Interpreter Error: HD called on empty sequence. ***\n"
  | TLEmptySeq => print "\n*** PLC Interpreter Error: TL called on empty sequence. ***\n"
  | ValueNotFoundInMatch => print "\n*** PLC Interpreter Error: Value not found in Match. ***\n"
  | NotAFunc => print "\n*** PLC Interpreter Error: Not a function. ***\n"
