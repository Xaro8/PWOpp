
(* The type of tokens. *)

type token = 
  | TRUE
  | THEN
  | SRPAREN
  | SLPAREN
  | RPAREN
  | PLUS
  | OR
  | NONE
  | NEQ
  | MULT
  | MINUS
  | LT
  | LPAREN
  | INT of (int)
  | IF
  | GT
  | FLOAT of (float)
  | FALSE
  | EQ
  | EOF
  | ENDL
  | ELT
  | ELSE
  | EGT
  | DIV
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stmt)
