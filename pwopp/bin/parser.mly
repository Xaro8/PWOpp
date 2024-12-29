%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENT

%token TRUE
%token FALSE
%token NONE

%token ASSGN

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
  | e = stmts; EOF { e }
  ;
block:
  | e = expr {Exp e}
  | SLPAREN; st = stmts; SRPAREN {st}
  ;
stmts:
  | st = stmt ; ENDL; {st}
  | st1 = stmt; ENDL; st2 = stmts {Seq (st1,st2)}
stmt:
  | e = expr {Exp e}
  | IF; e1 = expr; THEN; e2 = block ; ELSE; e3 = block  { If(e1, e2, e3) }
  | i = IDENT ; ASSGN ; e = expr { Assgn(i,e) }
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
  | i = IDENT { Var i }
  | LPAREN; e = expr; RPAREN { e }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | NONE { None }
  ;