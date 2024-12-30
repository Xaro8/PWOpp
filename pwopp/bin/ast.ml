(* abstract syntax tree *)

type op = Mult | Div | Add | Sub | Eq | Neq | Lt | Elt | Gt | Egt | And | Or

type expr =
  | Int of int
  | Var of string
  | Bool of bool
  | Float of float
  | Binop of expr * op * expr
  | None 
and stmt = 
  | Exp of expr
  | For of string * expr * expr * stmt
  | Assgn of string * expr
  | If of expr * stmt * stmt
  | Seq of stmt * stmt
                               
