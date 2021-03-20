(* PlcChecker *)
use "Environ.sml";
exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval(e, env: plcType): plcType =
    case e of
     ConI => IntT
    | ConB => BoolT
    | ESeq e => e
    | Var s => lookup(s, env)
    | Let (s, e1, e2) => if(getPlcType(teval(e1, env)) = getPlcType(teval(e2, env))) then teval(e2, env) else raise WrongRetType
    | Letrec (s1, pt1, s2, pt2, e1, e2) => if (getPlcType(teval(e1, env)) = pt2) then teval(e1, env) else raise WrongRetType
    | Prim1 (s, e) => case s of
          "!" => if (getType(e) = ConB) then teval(e, env) else raise WrongRetType
        | "-" => if (getType(e) = ConI) then teval(e, env) else raise WrongRetType
        | "hd" => if (getType(e) = List) then ListT(teval(e, env)) else raise WrongRetType
        | "tl" => if (getType(e) = List) then ListT(teval(e, env)) else raise WrongRetType
        | "ise" => if (getType(e) = List) then BoolT else raise WrongRetType
        | "print" => ListT
        | _ => raise UnknownType
    | Prim2 (s, e1, e2) => case s of
          "&&" => if (getType(e1) = getType(e2) andalso getType(e1) = ConB) then BoolT else raise NotEqTypes
        | "::" => if (getType(e1) = getType(e2) andalso getType(e1) = ConB) then ListT(teval(e, env)) else raise NotEqTypes
        | "+" => if (getType(e1) = getType(e2) andalso getType(e1) = ConI) then IntT else raise NotEqTypes
        | "-" => if (getType(e1) = getType(e2) andalso getType(e1) = ConI) then IntT else raise NotEqTypes
        | "/" => if (getType(e1) = getType(e2) andalso getType(e1) = ConI) then IntT else raise NotEqTypes
        | "*" => if (getType(e1) = getType(e2) andalso getType(e1) = ConI) then IntT else raise NotEqTypes
        | "<" => if (getType(e1) = getType(e2) andalso getType(e1) = ConI) then BoolT else raise NotEqTypes
        | "<=" => if (getType(e1) = getType(e2) andalso getType(e1) = ConI) then BoolT else raise NotEqTypes
        | "=" => if (getType(e1) = getType(e2)) then BoolT else raise NotEqTypes
        | "!=" => if (getType(e1) = getType(e2)) then BoolT else raise NotEqTypes
        | ";" => if (getType(e1) = getType(e2)) then teval(e1, env) else raise NotEqTypes
        | _ => raise UnknownType
    | If (t1, t2, t3) => if (getType(t1) = ConB) 
                            then 
                                if (getType(t2) = getType(t3)) 
                                    then
                                        getType(e)
                                    else
                                        raise DiffBrTypes
                            else 
                                raise IfCondNotBool
    | Match (e, list) =>
    | Call (e1, e2) => if (getType(e1) = Let) then teval(e1, env) else raise NotFunc
    | List e => ListT(e)
    | Item (i, e) => teval(e, env)
    | Anon (plcType, s, e) => if (getPlcType(teval(e, env)) = getPlcType(plcType) ) then teval(e, env) else raise WrongRetType
    | _ => raise UnknownType;

fun getType(e: expr) =
    case e of
    ConI => ConI
    | ConB => ConB
    | ESeq => ESeq
    | Var => Var
    | Let => Let
    | Letrec => Letrec
    | Prim1 => Prim1
    | Prim2 => Prim2
    | If => If
    | Match => Match
    | Call => Call
    | List => List
    | Item => Item
    | Anon => Anon
    | _ => raise UnknownType;

fun getPlcType(e: plcType):plcType =
    case e of 
    IntT => IntT
    | BoolT => BoolT
    | FunT  => FunT 
    | ListT => ListT
    | SeqT  => SeqT
    | _ => raise UnknownType;