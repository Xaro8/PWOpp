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
  | ArrayGet of string * expr list
  | Array_in of expr list * expr
  | None 
and stmt = 
  | Exp of expr
  | For of string * expr * expr * stmt
  | Assgn of string * expr
  | Assgn_arr of string * expr list * expr
  | Function of string * string list * stmt
  | Print of expr
  | Return of expr
  | Continue
  | Break
  | If of expr * stmt * stmt
  | Seq of stmt * stmt
                               
