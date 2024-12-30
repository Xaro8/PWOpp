(* abstract syntax tree *)

type op = Mult | Div | Add | Sub | Eq | Neq | Lt | Elt | Gt | Egt | And | Or

(*expressions retrun values and stmts do not*)
type expr =
  | Int of int
  | Var of string
  | Bool of bool
  | Float of float
  | Binop of expr * op * expr
  (* | Call of string * expr list *)
  | None 
and stmt = 
  | Exp of expr
  | For of string * expr * expr * stmt
  | Assgn of string * expr
  (* | Function of string * string list * stmt *)
  | Print of expr
  | If of expr * stmt * stmt
  | Seq of stmt * stmt
                               
