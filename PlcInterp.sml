(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr) (st:plcType env) : plcVal =
  case e of
      ConI(i) => IntV i
    | ConB(b) => BoolV v
    | ESeq(t) => SeqV []
    | Var(name) => eval (lookup st name) st
    | Let(name, value, exp) => eval exp ((name, (eval value st))::st)
    | Letrec(funcname, vartype, varname, functype, funcbody, prog) =>
      eval prog ((Clos (funcname, varname, funcbody, st))::st)
    | Prim1(op exp) => 
      let
        val value = eval exp st
      in
        case op of
          ("!") =>
            (case value of
              (BoolV v) => BoolV (not v)
              | _ => raise Impossible)
        | ("-") =>
            (case value of
              (IntV i) => IntV (~ i)
              | _ => raise Impossible)
        | ("hd") => 
          (case value of
             SeqV (xs::t) => xs
            | SeqV ([]) => raise HDEmptySeq
            | _ => raise Impossible)
        | ("tl") => 
          (case value of
             SeqV (xs::t) => t
            | SeqV ([]) => raise TLEmptySeq
            | _ => raise Impossible)
        | ("ise") =>
          (case value of
            SeqV ([]) => BoolV true
            | SeqV (xs::t) => BoolV false
            | _ => raise Impossible)
        | ("print") => print ((val2string value) ^ "\n")
        | _ => raise Impossible
      end
    | Prim2(op exp1 exp2) =>
      let
        val val1 = eval exp1 st;
        val val2 = eval exp2 st
      in
        case op of
          ("&&") =>
            (case (val1, val2) of
              (BoolV v1, BoolV v2) => BoolV (v1 andalso v2)
              | _ => raise Impossible)
        | ("+") =>
            (case (val1, val2) of
              (IntV v1, IntV v2) => IntV (v1 + v2)
              | _ => raise Impossible)
        | ("-") =>
            (case (val1, val2) of
              (IntV v1, IntV v2) => IntV (v1 - v2)
              | _ => raise Impossible)
        | ("*") =>
            (case (val1, val2) of
              (IntV v1, IntV v2) => IntV (v1 * v2)
              | _ => raise Impossible)
        | ("/") =>
            (case (val1, val2) of
              (IntV v1, IntV v2) => IntV (v1 div v2)
              | _ => raise Impossible)
        | ("=") =>
            (case (val1, val2) of
              (BoolV v1, BoolV v2) => BoolV (v1 = v2)
              | (IntV v1, IntV v2) => BoolV (v1 = v2)
              | (ListV v1, ListV v2) => BoolV (v1 = v2)
              | _ => raise Impossible)
        | ("!=") =>
            (case (val1, val2) of
              (BoolV v1, BoolV v2) => BoolV (v1 <> v2)
              | (IntV v1, IntV v2) => BoolV (v1 <> v2)
              | (ListV v1, ListV v2) => BoolV (v1 <> v2)
              | _ => raise Impossible)
        | ("<") =>
            (case (val1, val2) of
              (IntV v1, IntV v2) => IntV (v1 < v2)
              | _ => raise Impossible)
        | ("<=") =>
            (case (val1, val2) of
              (IntV v1, IntV v2) => IntV (v1 <= v2)
              | _ => raise Impossible)
        | ("::") =>
            (case val2 of
              (SeqV []) => SeqV [val1]
            | (SeqV v2) => SeqV (val1::v2)
            | _ => raise Impossible)
        | (";") => val2
        | _ => raise Impossible
      end
    | If(cond, thenexp, elsexp) =>
      let
        val test = eval cond st
      in
        case test of
          (BoolV result) => if result then (eval thenexp st) else (eval elsexp st)
        | _ => raise Impossible
      end
(* | Match of expr * (expr option * expr) list *)
    | Call(funcname, valexp) =>
      let
        val funcclosure = eval funcname st;
      in
        case funcclosure of 
          Clos(funcname, varname, funcbody, funcst) =>
            let
              val value = eval valexp st;
              val recst = (varname, value)::(funcname, funcclosure)::funcst
            in
              eval funcbody recst
            end
        | _ => raise NotAFunc
      end
    | List(items) =>
      let
        fun evalList (items:expr list) (st:plcType env) : plcVal list =
          case items of
            [] => []
          | xs::t => (eval xs st)::(evalList t st)
      in
        ListV (evalList items st)
      end
    | Item(index, itemexpr) =>
      let
        val itemeval = eval itemexpr st;
        fun findIndex (curr: int) (xs::t : plcVal list) : plcVal =
          if (curr = index) then xs else (if (curr > index orelse null t) then raise Impossible else findIndex (curr + 1) t)
      in
        case itemeval of
          ListV(items) => findIndex 1 items
        | _ => raise Impossible
      end
    | Anon(vartype, varname, funcbody) => Clos("", varname, funcbody, st);
