{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let number = '-'? digit+

rule token =
  parse
  | white { token lexbuf }
  | "!=" { NEQ }
  | ">" {GT}
  | ">=" {EGT}
  | "<" {LT}
  | "<=" {ELT}
  | "=" { EQ }
  | "&&" {AND}
  | "||" {OR}
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "*" { MULT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "None" { NONE }
  | number { INT (int_of_string (Lexing.lexeme lexbuf)) } 
  | eof { EOF }
