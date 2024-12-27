(* abstract syntax tree *)

type op = Mult | Div | Add | Sub | Eq | Neq | Lt | Elt | Gt | Egt
type lop = And | Or

type expr =
  | Int of int
  | Bool of bool
  (* | Float of float *)
  | Binop of expr * op * expr
  | If of expr * expr * expr
  | Logic of expr * lop * expr

                               
