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
let float_of_bool b = if b then 1.0 else 0.0 
let to_float = function 
  | VNone -> failwith "operation with None value"
  | VInt x -> float_of_int x
  | VBool b -> float_of_bool b
  | VFloat f -> f 
  | VFun _ -> raise Type_error
let string_of_value v =
  match v with
  | VNone      -> "None"
  | VInt n      -> string_of_int n
  | VBool true  -> "true"
  | VBool false -> "false"
  | VFloat f -> string_of_float f
  | VFun _ -> raise Type_error

let print_value v =
  print_endline (string_of_value v)
  
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
    let v, _ = eval_fun name args env in 
    v, env
  | Var x -> match M.find_opt x env with
    | Some v -> v, env
    | None -> raise (Unbound_var x)
and eval_stmt st env (cont: value*value M.t -> value * value M.t)= 
  match st with
  | Exp e -> cont (eval_exp e env)
  | Seq(st1,st2) -> eval_stmt st1 env (fun (_, env) -> eval_stmt st2 env cont) 
  | If(e,st1,st2) -> eval_stmt (Exp e) env (fun (v,env) -> 
    if (to_float v) <> 0.0 
      then eval_stmt st1 env cont 
      else eval_stmt st2 env cont
    )
  | Assgn (var,e) -> let x,env = eval_exp e env in cont (x,(M.add var x env))  
  | Print e -> let v,env =  eval_exp e env in 
    print_value v; cont (v,env) 
  | Function (name,args,body) -> cont (VFun (args,body),(M.add name (VFun (args,body)) env))
  | Return e -> cont (eval_exp e env)
  | For (var,starts,ends,st) -> 
    let stv  = eval_exp starts env in 
    let endv = eval_exp ends env in 
    match stv,endv with 
    | VInt i,VInt n -> eval_for var i n st env cont
    | _ -> raise Not_iterable
and eval_for var i n st (env : value M.t) (cont  : value -> value M.t -> value) : value = 
  if i < n - 1  then 
    let cont = fun _ env -> eval_stmt (For(var,Int (i+1), Int n, st)) env cont in 
    let env =  M.add var (VInt i) env
    in eval_stmt st env cont
  else if i = (n - 1) then 
    let env =  M.add var (VInt i) env
    in eval_stmt st env cont
  else VNone
and eval_fun name args env cont =
  match M.find_opt name env with 
  | Some (VFun(argnames,body) as fun_o) ->
    if List.length argnames <> List.length args then failwith "wrong number of arguments" else 
    let env' = M.add name fun_o M.empty in 
    M.
  | Some _ -> raise Type_error
  | None -> raise (Unbound_var name)
     
let eval_prog st = eval_stmt st (M.empty) (fun v _ -> v)
