open Ast
open Cf_monad2

exception Type_error
exception Not_iterable
exception Unbound_var of string

module M = Map.Make(String)

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
 | Add -> op_arith (+) (+.) v1 v2 |> return
 | Sub -> op_arith (-) (-.) v1 v2 |> return
 | Mult -> op_arith ( * ) ( *. ) v1 v2 |> return
 | Div -> op_arith (/) (/.) v1 v2 |> return
 | Eq -> op_eq (=) v1 v2 |> return
 | Lt -> op_eq (<) v1 v2 |> return
 | Gt -> op_eq (>) v1 v2 |> return
 | Elt -> op_eq (<=) v1 v2 |> return
 | Egt -> op_eq (>=) v1 v2 |> return
 | Neq -> op_eq (<>) v1 v2 |> return
 | _ -> raise Type_error
let rec string_of_value v = match v with
  | VNone       -> "None"
  | VInt n      -> string_of_int n
  | VBool true  -> "true"
  | VBool false -> "false"
  | VFloat f    -> string_of_float f
  | Varr a      -> "[" ^ (Array.to_list a |> List.map string_of_value |> String.concat ", ") ^ "]"
  | _           -> raise Type_error
let print_value v = v |> string_of_value |> print_endline
let rec eval_exp e = match e with  
  | Int a -> return (VInt a)
  | Bool b -> return (VBool b)
  | Float f -> return (VFloat f)
  | None -> return (VNone)
  | Binop (e1, op, e2) -> 
    eval_exp e1 >>= fun v1 ->
    eval_exp e2 >>= fun v2 ->
    eval_op op v1 v2
  | Call(name,args) -> eval_fun name args 
  | ArrayGet (name, idxs) -> 
    get_state >>=  fun env ->
    begin match M.find_opt name env with
      | Some (Varr a) -> array_op (Varr a) idxs
      | Some _ -> raise Type_error
      | None -> failwith ("Undefined array: " ^ name)
    end 
  | Array_in (vs, len) -> get_state >>= fun env -> array_init vs len env
  | Var x ->  
    get_state >>= fun env -> 
    match M.find_opt x env with
    | Some v -> return v
    | None -> raise (Unbound_var x) 
and eval_stmt = function 
  | Exp e -> eval_exp e 
  | Seq (st1, st2) -> eval_stmt st1 >>= fun _ -> eval_stmt st2  
  | If (e, st1, st2) -> 
    eval_exp e >>= fun v -> 
      if (to_float v) <> 0.0 then eval_stmt st1 else eval_stmt st2    
  | Assgn (var, e) -> 
    eval_exp e  >>= fun v ->
    get_state >>= fun env ->
    begin match v with
      | Varr arr -> let env = M.add var (Varr (Array.copy arr)) env in set_state env 
      | _ -> let env = M.add var v env in set_state env
    end 
  | Assgn_arr(name, idxs, e) ->   
    eval_exp e >>= fun v ->
    get_state >>= fun env -> 
    begin match M.find_opt name env with
      | Some (Varr a) -> array_op ?set:(Some v) (Varr a) idxs
      | Some _ -> raise Type_error
      | None -> failwith ("Undefined array: " ^ name)
    end 
  | Function (name, args, body) -> 
    get_state >>= fun env -> 
    let env = M.add name (VFun (args, body)) env in set_state env
  | Print e -> 
    eval_exp e >>= fun v -> 
    print_value v; return VNone
  (* | Return exp -> 
    eval_exp exp >>= fun v -> signal SReturn v  TODO *)
  | For (var, starts, ends, st) -> 
    eval_exp starts >>= fun stv ->
    eval_exp ends >>= fun endv ->
    begin match stv, endv with 
      | VInt i, VInt n -> eval_for var i n st
      | _ -> raise Not_iterable
    end
and eval_for var i n st=   
  if i < n then 
    get_state >>= fun env ->
    let env = M.add var (VInt i) env in
    set_state env >>= fun _ ->
    eval_stmt st >>= fun _  ->
    eval_for var (i + 1) n st 
  else return VNone
and eval_fun name args = 
  get_state >>= fun env ->
  match M.find_opt name env with 
  | Some (VFun (args_n, body) as f_o) -> 
    let env' =  declare_env args_n args env M.empty in 
    let env' =  M.add name f_o env' in
    set_state env' >>= fun _ -> 
    eval_stmt body >>= fun _ -> set_state env 
  | Some _ -> raise Type_error
  | None   -> raise (Unbound_var name)

and declare_env args_n args env ret : value M.t = 
  match args_n, args with
  | n :: args_n, v :: args -> 
    let v = run (eval_exp v) env in
    let ret =  M.add n v ret in
    declare_env args_n args env ret 
  | [], [] -> ret
  | _ , [] -> failwith "insufficient number of arguments given"
  | [], _ -> failwith "too much arguments given"
and array_init vs len env = 
  let help v ret = 
    let v = run (eval_exp v) env in v :: ret
  in let ret = List.fold_right help vs [] 
  in let retlen = List.length ret 
  in begin match run (eval_exp len) env with
    | VInt l -> Varr (Array.init (l * retlen) (fun idx -> List.nth ret (idx mod retlen))) |> return
    | _ -> raise Type_error
  end 
and array_op ?set:(set=VNone) arr idxs = 
  let help idx arr =
    match idx, arr with
    | VInt i, Varr arr -> arr.(i)
    | VInt _, _ -> failwith "this type is not callable" 
    | _ -> raise Type_error 
  in
  get_state >>= fun env -> 
  match idxs with 
  | [] -> return arr
  | [idx] when set <> VNone -> 
    eval_exp idx >>= fun i ->
    begin match i, arr, set with
      | VInt i, Varr arr, Varr v -> 
        arr.(i) <- Varr (Array.copy v); 
        set_state env
      | VInt i, Varr arr, _ -> arr.(i) <- set;  set_state env
      | VInt _, _ , _ -> failwith "this type is not callable"
      | _ -> failwith "Index should be 'int' type"
    end 
  | idx :: idxs -> 
    eval_exp idx >>= fun idx ->
    array_op (help idx arr) idxs

let eval_prog st = eval_stmt st