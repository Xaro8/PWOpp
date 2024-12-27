%{
open Ast
%}

%token <int> INT
%token TRUE
%token FALSE

%token MULT
%token DIV
%token PLUS
%token MINUS

%token AND
%token OR

%token EQ
%token NEQ
%token LT
%token ELT
%token GT
%token EGT

%token LPAREN
%token RPAREN

%token IF
%token THEN
%token ELSE

%token EOF

%start <Ast.expr> prog


%left PLUS MINUS
%left MULT DIV

%%

prog:
  | e = mixfix; EOF { e }
  ;

mixfix:
  | IF; e1 = mixfix; THEN; e2 = mixfix; ELSE; e3 = mixfix { If(e1, e2, e3) }
  | e = expr { e }
  ;

expr:
  | e =  base { e }
  | e1 = expr; PLUS; e2 = expr { Binop(e1, Add, e2) }
  | e1 = expr; MINUS; e2 = expr { Binop(e1, Sub, e2) }
  | e1 = expr; DIV; e2 = expr { Binop(e1, Div, e2) }
  | e1 = expr; MULT; e2 = expr { Binop(e1, Mult, e2) }
  | e1 = expr; EQ; e2 = expr { Binop(e1, Eq, e2) }
  | e1 = expr; AND; e2 = expr { Logic(e1, And, e2) }
  | e1 = expr; OR; e2 = expr { Logic(e1, Or, e2) }
  | e1 = expr; NEQ; e2 = expr { Binop(e1, Neq, e2) }
  | e1 = expr; LT; e2 = expr { Binop(e1, Lt, e2) }
  | e1 = expr; ELT; e2 = expr { Binop(e1, Elt, e2) }
  | e1 = expr; GT; e2 = expr { Binop(e1, Gt, e2) }
  | e1 = expr; EGT; e2 = expr { Binop(e1, Egt, e2) }
  ;
base:
  | i = INT { Int i }
  | LPAREN; e = mixfix; RPAREN { e }
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;