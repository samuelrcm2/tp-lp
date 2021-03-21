%%

%name PlcParser

%pos int

%term SEMIC | COL
    | ARROW | DBLARROW
    | VAR | ANONFUN | FUN | REC | END
    | IF | THEN | ELSE
    | MATCH | WITH | PIPE | UNDSCR
    | NOT | HD | TL | ISE | PRINT
    | AND | PLUS | MINUS | MULT | DIV | EQ | DIF | LESS | LESSEQ | CONCAT
    | LPAR | RPAR | LBRAC | RBRAC | LCBR | RCBR
    | TRUE | FALSE
    | COMMA
    | TNIL | TBOOL | TINT
    | NAME of string | NAT of int
    | EOF

%nonterm Prog of expr
    | Decl of expr
    | Expr of expr
    | AtomExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomType of plcType
    | Types of plcType list

%right SEMIC ARROW 
%nonassoc IF
%left ELSE
%left AND
%left EQ DIF
%left LESS LESSEQ
%right CONCAT 
%left PLUS MINUS
%left MULT DIV
%nonassoc NOT HD TL ISE PRINT NAME
%left LBRAC

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
    | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
    | FUN REC NAME Args COL Type EQ Expr SEMIC Prog (makeFun(NAME, Args, Type, Expr, Prog))

Expr : AtomExpr (AtomExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | NOT Expr (Prim1("!", Expr1))
    | MINUS Expr (Prim1("-", Expr1))
    | HD Expr (Prim1("hd", Expr1))
    | TL Expr (Prim1("tl", Expr1))
    | ISE Expr (Prim1("ise", Expr1))
    | PRINT Expr (Prim1("print", Expr1))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULT Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr DIF Expr (Prim2("!=", Expr1, Expr2))
    | Expr LESS Expr (Prim2("<", Expr1, Expr2))
    | Expr LESSEQ Expr (Prim2("<=", Expr1, Expr2))
    | Expr CONCAT Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LBRAC NAT RBRAC (Item(NAT, Expr))

AtomExpr : Const (Const)
    | NAME (Var NAME)
    | LCBR Prog RCBR (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (List(Comps))
    | ANONFUN Args DBLARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const : TRUE (ConB true)
    | FALSE (ConB false)
    | NAT (ConI NAT)
    | LPAR RPAR (List [])
    | LPAR Type LBRAC RBRAC RPAR (ESeq(Type))

Comps : Expr COMMA Expr (Expr1::Expr2::[])
    | Expr COMMA Comps (Expr::Comps)

MatchExpr : END ([])
    | PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : UNDSCR (NONE)
    | Expr (SOME(Expr))

Args : LPAR RPAR ([])
    | LPAR Params RPAR (Params)

Params : TypedVar (TypedVar::[])
    | TypedVar COMMA Params (TypedVar::Params)

TypedVar : Type NAME (Type, NAME)

Type : AtomType (AtomType)
    | LPAR Types RPAR (ListT(Types))
    | LBRAC Type RBRAC (SeqT(Type))
    | Type ARROW Type (FunT(Type1, Type2))

AtomType : TNIL (ListT [])
    | TBOOL (BoolT)
    | TINT (IntT)
    | LPAR Type RPAR (Type)

Types : Type COMMA Type (Type1::Type2::[])
    | Type COMMA Types (Type::Types)
