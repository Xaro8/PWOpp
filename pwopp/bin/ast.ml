(* abstract syntax tree *)

type op = Mult | Div | Add | Sub | Eq | Neq | Lt | Elt | Gt | Egt | And | Or

(*expressions retrun values and stmts do not*)
type expr =
  | Int of int
  | Var of string
  | Bool of bool
  | Float of float
  | Binop of expr * op * expr
  | Call of string * expr list
  | ArrayGet of string * expr
  | Array_in of expr
  | None 
and stmt = 
  | Exp of expr
  | For of string * expr * expr * stmt
  | Assgn of string * expr
  | Assgn_arr of string * expr * expr
  | Function of string * string list * stmt
  | Print of expr
  | Return of expr
  | If of expr * stmt * stmt
  | Seq of stmt * stmt
                               
