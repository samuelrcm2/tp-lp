(* Plc interpreter main file *)

fun run (exp: expr) : string =
  let
    val exptype = teval exp [];
    val expval = eval exp []
  in
    (val2string expval) ^ " : " ^ (type2string exptype)
  end
  handle
    SymbolNotFound => "\n*** PLC Environment Error: Symbol not found. ***\n"
  | EmptySeq => "\n*** PLC Type Error: Empty input sequence. ***\n"
  | UnknownType => "\n*** PLC Type Error: Unknown type. ***\n"
  | NotEqTypes => "\n*** PLC Type Error: Type mismatch in comparison. ***\n"
  | WrongRetType => "\n*** PLC Type Error: Declared return type does not match function body. ***\n"
  | DiffBrTypes => "\n*** PLC Type Error: Type mismatch on If branches bodies. ***\n"
  | IfCondNotBool => "\n*** PLC Type Error: Type mismatch on If condition; Condition must be Bool. ***\n"
  | NoMatchResults => "\n*** PLC Type Error: No Match results. ***\n"
  | MatchResTypeDiff => "\n*** PLC Type Error: Type mismatch on Match options' bodies. ***\n"
  | MatchCondTypesDiff => "\n*** PLC Type Error: Type mismatch on Match options' conditions. ***\n"
  | CallTypeMisM => "\n*** PLC Type Error: Type mismatch on function call. ***\n"
  | NotFunc => "\n*** PLC Type Error: Not a function. ***\n"
  | ListOutOfRange => "\n*** PLC Type Error: Attempting to access an item out of range. ***\n"
  | OpNonList => "\n*** PLC Type Error: Attempting to access an item but expression is not a list. ***\n"
  | Impossible => "\n*** PLC Interpreter Error: Impossible operation. ***\n"
  | HDEmptySeq => "\n*** PLC Interpreter Error: HD called on empty sequence. ***\n"
  | TLEmptySeq => "\n*** PLC Interpreter Error: TL called on empty sequence. ***\n"
  | ValueNotFoundInMatch => "\n*** PLC Interpreter Error: Value not found in Match. ***\n"
  | NotAFunc => "\n*** PLC Interpreter Error: Not a function. ***\n"