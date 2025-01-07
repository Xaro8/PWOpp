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

%token FOR
%token TO
%token COLON
%token PRINT

%token DEF
%token RETURN

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
  | s = stmt {s} 
  | SLPAREN; st = stmts; SRPAREN {st}
  ;
  
idents:
  | x = IDENT; ENDL ; xs = idents { x :: xs }
  | x = IDENT { [x] }
  ;

stmts:
  | st = stmt ; {st}
  | st = stmt ; ENDL; {st}
  | st1 = stmt; ENDL; st2 = stmts {Seq (st1,st2)}
stmt:
  | e = expr {Exp e}
  | IF; e1 = expr; THEN; b1 = block ; ELSE; b2 = block  { If(e1, b1, b2) }
  | i = IDENT ; ASSGN ; e = expr { Assgn(i,e) }
  | FOR; i = IDENT; ASSGN; e1 = expr; TO; e2 = expr ; COLON ; b = block{ For(i, e1, e2, b) } 
  | PRINT; e = expr ; {Print e}
  | DEF; i = IDENT; LPAREN ; args = idents ; RPAREN ; COLON ; b = block { Function(i, args, b)}
  | RETURN; e = expr ; {Return e}
  ;

exprs: 
  | e = expr; ENDL; xs = exprs { e :: xs }
  | e = expr { [e] }
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
  | i = IDENT; LPAREN ; arg = exprs ; RPAREN {Call(i,arg)}
  | LPAREN; e = expr; RPAREN { e }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | NONE { None }
  ;