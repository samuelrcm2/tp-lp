(* PlcChecker *)
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

fun teval(e: expr) (env: plcType env) : plcType =
    case e of
     ConI i => IntT 
    | ConB b => BoolT
    | ESeq e => e
    | Var s => lookup env s
    | Let (s, e1, e2) =>  if((teval e1 env) = (teval e2 env)) then teval e2 env else raise WrongRetType
    | Letrec (s1, pt1, s2, pt2, e1, e2) =>
        let
            val funcenv = (s2, pt1)::env
        in
            if ((teval e1 funcenv) = pt2) then teval e1 funcenv else raise WrongRetType
        end
    | Prim1 (s, e) => let
                val ee = teval e env
            in
                case s of
                    "!" => if ((ee) = BoolT) then ee else raise WrongRetType
                    | "-" => if ((ee) = IntT) then ee else raise WrongRetType
                    | "hd" => let
                                val evaltype = ee
                            in
                                case evaltype of
                                ListT(types) => evaltype
                                | _ => raise WrongRetType
                            end
                    | "tl" => let
                                val evaltype = ee
                            in
                                case evaltype of
                                ListT(types) => evaltype
                                | _ => raise WrongRetType
                            end
                    | "ise" => let
                                val evaltype = ee
                            in
                                case evaltype of
                                ListT(types) => evaltype
                                | _ => raise WrongRetType
                            end
                    | "print" => ee
                    | _ => raise UnknownType
                    end
    | Prim2 (s, e1, e2) => 
            let
                val ee1 = teval e1 env;
                val ee2 = teval e2 env
            in case s of
                ("&&") => if ((ee1 = ee2) andalso (ee1 = BoolT)) then BoolT else raise NotEqTypes
                | ("::") => if ((ee1 = ee2) andalso (ee1 = BoolT)) then ee1 else raise NotEqTypes
                | ("+") => if ((ee1 = ee2) andalso (ee1 = IntT)) then IntT else raise NotEqTypes
                | ("-") => if ((ee1 = ee2) andalso (ee1 = IntT)) then IntT else raise NotEqTypes
                | ("/") => if ((ee1 = ee2) andalso (ee1 = IntT)) then IntT else raise NotEqTypes
                | ("*") => if ((ee1 = ee2) andalso (ee1 = IntT)) then IntT else raise NotEqTypes
                | ("<") => if ((ee1 = ee2) andalso (ee1 = IntT)) then BoolT else raise NotEqTypes
                | ("<=") => if ((ee1 = ee2) andalso (ee1 = IntT)) then BoolT else raise NotEqTypes
                | ("=") => if (ee1 = ee2) then BoolT else raise NotEqTypes
                | ("!=") => if (ee1 = ee2) then BoolT else raise NotEqTypes
                | (";") => if (ee1 = ee2) then ee1 else raise NotEqTypes
                | _ => raise UnknownType
            end
    | If (t1, t2, t3) => if ((teval t1 env) = BoolT) 
                        then 
                        if ((teval t2 env) = (teval t3 env))
                            then
                                teval t2 env
                            else
                                raise DiffBrTypes
                        else 
                            raise IfCondNotBool
    | Call (e1, e2) => let
            val ee1 = e1
            in
        case ee1 of
        Let(s, e1, e2) => teval e1 env
        | _ => raise NotFunc
        end
    | Match (e, list) => let
        val llist = list
        in
            case llist of 
            (a::xs) => let
                    fun checkMatch(next::rest: (expr option * expr) list)(env: (string *plcType )list): int = 
                        case next of
                            (SOME(c), e) =>
                            let
                                val nextvalue = hd rest
                            in
                                case nextvalue of
                                (SOME(c1), e1) => if ((teval c env) = (teval c1 env)) 
                                                then
                                                    if ((teval e env) = (teval e1 env))
                                                    then
                                                        checkMatch rest env
                                                    else
                                                        raise MatchResTypeDiff
                                                else
                                                    raise MatchCondTypesDiff
                                | (NONE, e1) => if ((teval e env) = (teval e1 env)) 
                                                then       
                                                    checkMatch rest env
                                                else
                                                    raise MatchResTypeDiff
                            end
                            | (NONE, e) =>
                                let
                                    val nextvalue = hd rest
                                in case nextvalue of
                                (SOME (c1), e1) => if ((teval e env) = (teval e1 env))
                                                then
                                                    checkMatch rest env
                                                else
                                                    raise MatchResTypeDiff
                                | (NONE, e1) => raise WrongRetType
                                end
                    val verifyList = checkMatch list env
                in
                    case a of
                    (SOME c1, e1) => teval e1 env
                    | (NONE, e1) => teval e1 env
                end
            | _ => raise NoMatchResults
        end
    | Item (i, e) =>
        let
            val evaltype = teval e env
        in
            case evaltype of
                ListT(types) => evaltype
                | _ => raise OpNonList
        end
    | Anon (plcType, s, e) => if ((teval e env) = plcType ) then teval e env else raise WrongRetType
    | List e => case e of
        (hd::tl) => teval hd env 
        | _ => raise OpNonList
    ;