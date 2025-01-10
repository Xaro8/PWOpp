open Ast
exception Type_error
exception Not_iterable
exception Unbound_var of string
module M = Map.Make(String)

type value =
  | VNone
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VFun of string list * Ast.stmt

exception Return_ex of value * value M.t (*mozna zmienic*)

let float_of_bool b = if b then 1.0 else 0.0 
let to_float = function 
  | VNone -> failwith "operation with None value"
  | VInt x -> float_of_int x
  | VBool b -> float_of_bool b
  | VFloat f -> f 
  | _ -> raise Type_error

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
let string_of_value v =
  match v with
  | VNone      -> "None"
  | VInt n      -> string_of_int n
  | VBool true  -> "true"
  | VBool false -> "false"
  | VFloat f -> string_of_float f
  | _ -> raise Type_error


let print_value v =
  print_endline (string_of_value v)

let rec eval_exp e env =
  match e with  
  | Int a -> VInt a, env
  | Bool b -> VBool b, env 
  | Float f -> VFloat f, env
  | None -> VNone, env
  | Binop (e1, op, e2) -> 
    let v1, env = eval_exp e1 env in 
    let v2, env = eval_exp e2 env in
    eval_op op v1 v2, env
  | Call(name,args) -> 
    let v,_ = eval_fun name args env in 
     v, env
  | Var x -> match M.find_opt x env with
    | Some v -> v, env
    | None -> raise (Unbound_var x)

and eval_stmt st env = 
  match st with
  | Exp e -> snd (eval_exp e env)
  | Seq(st1,st2) -> eval_stmt st2 (eval_stmt st1 env) 
  | If(e,st1,st2) -> 
    let v,env = eval_exp e env in
    if (to_float v) <> 0.0 then eval_stmt st1 env else eval_stmt st2 env 
  | Assgn (var,e) -> 
    let v, env = eval_exp e env in 
    (M.add var v env)  
  | Function (name,args,body) -> M.add name (VFun(args,body)) env  
  | Print e -> 
    let v, env = eval_exp e env in 
    print_value v; env
  | Return exp -> 
      let v, env  = eval_exp exp env in 
      raise (Return_ex (v,env))
  | For (var,starts,ends,st) -> 
    let stv, env = eval_exp starts env in 
    let endv, env = eval_exp ends env in 
    match stv,endv with 
    | VInt i, VInt n -> eval_for var i n st env
    | _ -> raise Not_iterable
and eval_for var i n st (env : value M.t) =   
  if i < n then 
    let env = M.add var (VInt i) env in
    eval_for var (i+1) n st (eval_stmt st env)
  else env
and eval_fun name args env = 
  match M.find_opt name env with 
  | Some (VFun(args_n,body) as f_o) -> 
    let env' =  declare_env args_n args env M.empty in 
    let env' =  M.add name f_o env' in
    begin try VNone, eval_stmt body env' with Return_ex (v,env) -> v,env end 
  | Some _ -> raise Type_error
  | None -> raise (Unbound_var name)
and declare_env args_n args env ret = 
  match args_n,args with
  | n :: args_n, v :: args -> 
    let v,env =  eval_exp v env in 
    let ret = M.add n v ret in 
    declare_env args_n args env ret 
  | [], [] -> ret
  | _ , [] -> failwith "insufficient number of arguments given"
  | [], _ -> failwith "too much arguments given"
let eval_prog st = eval_stmt st (M.empty)
