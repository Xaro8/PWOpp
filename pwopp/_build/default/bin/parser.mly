%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token TRUE
%token FALSE
%token NONE

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
%token SLPAREN
%token SRPAREN

%token IF
%token THEN
%token ELSE

%token ENDL
%token EOF

%start <Ast.stmt> prog


%left PLUS MINUS
%left MULT DIV

%%

prog:
  | e = stmt; EOF { e }
  ;
block:
  | e = expr {Exp e}
  | SLPAREN; st = stmt; SRPAREN {st}
  ;
stmt:
  | e = expr; ENDL; {Exp e}
  | st1 = stmt; ENDL; st2 = stmt {Seq (st1,st2)}
  | e = expr; ENDL; st2 = stmt {Seq (Exp(e),st2)}
  | IF; e1 = expr; THEN; e2 = block ; ELSE; e3 = block  { If(e1, e2, e3) }
  ;

expr:
  | e =  base { e }
  | e1 = expr; PLUS; e2 = expr { Binop(e1, Add, e2) }
  | e1 = expr; MINUS; e2 = expr { Binop(e1, Sub, e2) }
  | e1 = expr; DIV; e2 = expr { Binop(e1, Div, e2) }
  | e1 = expr; MULT; e2 = expr { Binop(e1, Mult, e2) }
  | e1 = expr; EQ; e2 = expr { Binop(e1, Eq, e2) }
  | e1 = expr; AND; e2 = expr { Binop(e1, And, e2) }
  | e1 = expr; OR; e2 = expr { Binop(e1, Or, e2) }
  | e1 = expr; NEQ; e2 = expr { Binop(e1, Neq, e2) }
  | e1 = expr; LT; e2 = expr { Binop(e1, Lt, e2) }
  | e1 = expr; ELT; e2 = expr { Binop(e1, Elt, e2) }
  | e1 = expr; GT; e2 = expr { Binop(e1, Gt, e2) }
  | e1 = expr; EGT; e2 = expr { Binop(e1, Egt, e2) }
  ;
  
base:
  | i = INT { Int i }
  | f = FLOAT { Float f}
  | LPAREN; e = expr; RPAREN { e }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | NONE { None }
  ;