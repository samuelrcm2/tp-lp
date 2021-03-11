(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

fun keyWord (s, lpos, rpos) =
    case s of 
        "var" => VAR (lpos, rpos)
        | "Bool" => TBOOL (lpos, rpos)
        | "else" => ELSE (lpos, rpos)
        | "end" => END (lpos, rpos)
        | "false" => FALSE (lpos, rpos)
        | "fn" => ANONFUN (lpos, rpos)
        | "fun" => FUN (lpos, rpos)
        | "hd" => HD (lpos, rpos)
        | "if" => IF (lpos, rpos)
        | "Int" => TINT (lpos, rpos)
        | "ise" => ISE (lpos, rpos)
        | "match" => MATCH (lpos, rpos)
        | "Nil" => TNIL (lpos, rpos)
        | "print" => PRINT (lpos, rpos)
        | "rec" => REC (lpos, rpos)
        | "then" => THEN (lpos, rpos)
        | "tl" => TL (lpos, rpos)
        | "true" => TRUE (lpos, rpos)
        | "with" => REC (lpos, rpos)
        | _   => NAME (s, lpos, rpos)

fun strToInt s =
    case Int.fromString s of
        SOME i =: i
    |   NONE => raise Fail ("Could not convert string '" ^ s ^ "' to integer")

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;
%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => (TINT(strToInt(yytext), yypos, yypos));
{identifier} => (keyWord(yytext, yypos, yypos));
"+" => (PLUS(yypos, yypos))
"-" => (MINUS(yypos, yypos))
"*" => (MULT(yypos, yypos))
"/" => (DIV(yypos, yypos))
"(" => (LPAR(yypos, yypos))
")" => (RPAR(yypos, yypos))
"=" => (EQ(yypos, yypos))
";" => (SEMIC(yypos, yypos))
"&&" => (AND(yypos, yypos))
"!=" => (DIF(yypos, yypos))
"::" => (CONCAT(yypos, yypos))
"<" => (LESS(yypos, yypos))
"<=" => (LESSEQ(yypos, yypos))
"{" => (LBRAC(yypos, yypos))
"}" => (RBRAC(yypos, yypos))
"[" => (LCBR(yypos, yypos))
"]" => (RCBR(yypos, yypos))
"," => (COMMA(yypos, yypos))
"!" => (NOT(yypos, yypos))
"|" => (PIPE(yypos, yypos))
, => (error("\n***Lexer errorbad character ***\n");
    raise Fail("Lexer error: bad character " ^yytext));