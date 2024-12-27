
(* The type of tokens. *)

type token = 
  | TRUE
  | THEN
  | RPAREN
  | PLUS
  | OR
  | NEQ
  | MULT
  | MINUS
  | LT
  | LPAREN
  | INT of (int)
  | IF
  | GT
  | FALSE
  | EQ
  | EOF
  | ELT
  | ELSE
  | EGT
  | DIV
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
