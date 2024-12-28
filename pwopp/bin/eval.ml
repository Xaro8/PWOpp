open Ast
exception Type_error

type value =
  | VNone
  | VInt of int
  | VBool of bool
  | Vfloat of float
let float_of_bool b = if b then 1.0 else 0.0 
let to_float = function 
  | VNone -> failwith "operation with None value"
  | VInt x -> float_of_int x
  | VBool b -> float_of_bool b
  | Vfloat f -> f 

let op_arith f_int _ v1 v2 =
  match v1, v2 with
  | VInt x, VInt y -> VInt (f_int x y)
  (* | VFloat x, VFloat y -> VFloat (f_float x y)
  | VInt x, VFloat y -> VFloat (f_float (float_of_int x) y)
  | VFloat x, VInt y -> VFloat (f_float x (float_of_int y)) *)
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
  | None -> VNone
  | Binop(e1,op,e2) -> eval_op op (eval_exp e1) (eval_exp e2)
  | If (e,t,f) -> 
    match eval_exp e with
    | VBool b -> if b then eval_block t else eval_block f
    | VNone -> eval_block f (*in this language None has boolen value = false*)
    | VInt a  -> if a <> 0  then eval_block t else eval_block f  (*same goes with zero*)
    | Vfloat a -> if a <> 0.0 then eval_block t else eval_block f
and eval_block b  =  eval_exp b 
let eval_prog p = eval_block p


let print_value _ = print_newline()
