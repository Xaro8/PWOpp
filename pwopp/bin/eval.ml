open Ast
exception Type_error

type value =
  | VNone
  | VInt of int
  | VBool of bool
  | VFloat of float
let float_of_bool b = if b then 1.0 else 0.0 
let to_float = function 
  | VNone -> failwith "operation with None value"
  | VInt x -> float_of_int x
  | VBool b -> float_of_bool b
  | VFloat f -> f 

let op_arith f_int f_float v1 v2 =
  match v1, v2 with
  | VInt x, VInt y -> VInt (f_int x y)
  | VFloat x, VFloat y -> VFloat (f_float x y)
  | VInt x, VFloat y -> VFloat (f_float (float_of_int x) y)
  | VFloat x, VInt y -> VFloat (f_float x (float_of_int y))
  | _ -> raise Type_error

let op_eq f v1 v2 = VBool (f ( to_float v1) (to_float v2)) 
 
let eval_op op v1 v2 = match op with 
 | Add -> op_arith (+) (+.) v1 v2
 | Sub -> op_arith (-) (-.) v1 v2
 | Mult -> op_arith ( * ) ( *. ) v1 v2
 | Div -> op_arith (/) (/.) v1 v2
 | Eq -> op_eq (=) v1 v2
 | Lt -> op_eq (<) v1 v2
 | Gt -> op_eq (>) v1 v2
 | Elt -> op_eq (<=) v1 v2
 | Egt -> op_eq (>=) v1 v2
 | Neq -> op_eq (<>) v1 v2
 | _ -> raise Type_error

let rec eval_exp =  function 
  | Int a -> VInt a
  | Bool b -> VBool b   
  | Float f -> VFloat f
  | None -> VNone
  | Binop(e1,op,e2) -> eval_op op (eval_exp e1) (eval_exp e2)

let rec eval_stmt st cont = 
  match st with
  | Exp e -> cont (eval_exp e)
  | Seq(st1,st2) -> eval_stmt (st1) (fun _ -> eval_stmt st2 cont)
  | If(e,st1,st2) -> eval_stmt (Exp e) (fun v -> if (to_float v) <> 0.0 then eval_stmt st1 cont else eval_stmt st2 cont)

let eval_prog st = eval_stmt st Fun.id
let string_of_value v =
  match v with
  | VNone      -> "None"
  | VInt n      -> string_of_int n
  | VBool true  -> "true"
  | VBool false -> "false"
  | VFloat f -> string_of_float f

let print_value v =
  print_endline (string_of_value v)
