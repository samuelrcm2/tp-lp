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

%nonterm Prog of (* WIP *)
    | Decl of (* WIP *)
    | Expr of (* WIP *)
    | AtomExpr of (* WIP *)
    | AppExpr of (* WIP *)
    | Const of (* WIP *)
    | Comps of (* WIP *)
    | MatchExpr of (* WIP *)
    | CondExpr of (* WIP *)
    | Args of (* WIP *)
    | Params of (* WIP *)
    | TypedVar of (* WIP *)
    | Type of (* WIP *)
    | AtomType of (* WIP *)
    | Types of (* WIP *) 

%right SEMIC ARROW 
%nonassoc IF
%left ELSE AND EQ DIF LESS LESSEQ
%right CONCAT 
%left PLUS MINUS MULT DIV
%nonassoc NOT HD TL ISE PRINT 
%left LBRAC

%eop EOF

%noshift EOF

%start Prog

%%

Prog : (* WIP *)

Decl : (* WIP *)

Expr : (* WIP *)

AtomExpr : (* WIP *)

AppExpr : (* WIP *)

Const : (* WIP *)

Comps : (* WIP *)

MatchExpr : (* WIP *)

CondExpr : (* WIP *)

Args : (* WIP *)

Params : (* WIP *)

TypedVar : (* WIP *)

Type : (* WIP *)

AtomType : (* WIP *)

Types : (* WIP *) 
