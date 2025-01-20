{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let number = '-'? digit+
let float =  '-'? digit+ '.' digit+
let ident = letter (letter | digit)*



rule token =
  parse
  | '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | white { token lexbuf }
  | "!=" { NEQ }
  | ">" {GT}
  | ">=" {EGT}
  | "<" {LT}
  | "<=" {ELT}
  | "=" { EQ }
  | "&&" {AND}
  | "||" {OR}
  | "*" { MULT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { SLPAREN }
  | "}" { SRPAREN }
  | "[" { SQLPAREN }
  | "]" { SQRPAREN }
  | ";" { ENDL }
  | ":" { COLON }
  | "," { COMMA }
  | "for" {FOR}
  | "while" {WHILE}
  | "to" {TO}
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "return" {RETURN}
  | "continue" { CONTINUE }
  | "break" { BREAK}
  | "def" {DEF}
  | "print" { PRINT }
  | ":=" { ASSGN }
  | "none" { NONE }
  | number { INT (int_of_string (Lexing.lexeme lexbuf)) } 
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
