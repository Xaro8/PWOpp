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
  | "{" { SLPAREN }
  | "}" { SRPAREN }
  | ";" { ENDL }
  | ":=" { ASSGN }
  | "None" { NONE }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | number { INT (int_of_string (Lexing.lexeme lexbuf)) } 
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
