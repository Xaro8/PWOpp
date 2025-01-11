module Operations = struct
  type value =
  | VNone
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VFun of string list * Ast.stmt
  | Varr of value array

end