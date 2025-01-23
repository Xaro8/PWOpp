open PwoLib
open Ast

type value =
  | Undef
  | Num     of int
  | Float   of float
  | Addr    of int
  | RetAddr of string option * Ast.cmd list

let runtime_error ?msg () =
  begin match msg with
  | None     -> Printf.eprintf "Runtime error!\n"
  | Some msg -> Printf.eprintf "Runtime error: %s\n" msg
  end;
  raise Fatal_error

let vm_assert b =
  if b then ()
  else runtime_error ()

let wrap_bool b =
  Float (if b then 1.0 else 0.0)

let to_float = function 
  | Undef -> runtime_error()
  | Num x -> float_of_int x
  | Float f -> f 
  | _ -> runtime_error()
let op_arith f_int f_float v1 v2 =
  match v1, v2 with
  | Num x, Num y -> Num (f_int x y)
  | Float x, Float y -> Float (f_float x y)
  | Num x, Float y -> Float (f_float (float_of_int x) y)
  | Float x, Num y -> Float (f_float x (float_of_int y))
  | _ -> runtime_error()
let op_eq f v1 v2 = (f ( to_float v1) (to_float v2))  
let eval_op op v1 v2 = match op with 
| Add -> op_arith (+) (+.) v1 v2
| Sub -> op_arith (-) (-.) v1 v2
| Mult -> op_arith ( * ) ( *. ) v1 v2
| Div -> op_arith (/) (/.) v1 v2
| Eq -> op_eq (=) v1 v2 |> wrap_bool
| Lt -> op_eq (<) v1 v2 |> wrap_bool 
| Gt -> op_eq (>) v1 v2 |> wrap_bool
| Elt -> op_eq (<=) v1 v2 |> wrap_bool
| Egt -> op_eq (>=) v1 v2 |> wrap_bool
| Neq -> op_eq (<>) v1 v2 |> wrap_bool
| _ -> runtime_error()
(* ========================================================================= *)
(* VM *)

let rec run_loop (curf : 'a option) (cmds : Ast.cmd list) stack acc =
  match cmds, stack, acc with
  | [], _, _ -> 
  begin match curf with
    | None      -> ()
    | Some name ->
      runtime_error ~msg:("missing RET command in function " ^ name) ()
  end

  | TOP :: cmds, _, _ ->run_loop curf cmds stack (Addr (stack_top stack))
  | LEA n :: cmds, _, Addr a ->
    vm_assert (n >= 0 && n <= a);
    run_loop curf cmds stack (Addr (a - n))
  | LEA _ :: _, _, _ -> runtime_error ()

  | ENTER n :: cmds, _, _ ->
    vm_assert (n >= 0);
    run_loop curf cmds (stack_enter ~dflt:Undef n stack) acc

  | LEAVE n :: cmds, _, _ ->
    vm_assert (n >= 0 && n <= stack_size stack);
    run_loop curf cmds (stack_leave n stack) acc

  | PUSH :: cmds, _, _ ->
    run_loop curf cmds (acc :: stack) acc

  | LOAD n :: cmds, _, Addr a ->
    vm_assert (n >= 0 && n < a && a <= stack_size stack);
    run_loop curf cmds stack (stack_load (a - n) stack)
  | LOAD _ :: _, _, _ -> runtime_error ()

  | STORE n :: cmds, Addr a :: stack, _ ->
    vm_assert (n >= 0 && n < a && a <= stack_size stack);
    run_loop curf cmds (stack_store (a - n) acc stack) acc
  | STORE _ :: _, _, _ -> runtime_error ()

  | CONSTN n :: cmds, _, _ -> run_loop curf cmds stack (Num n)
  | CONSTF n :: cmds, _, _ -> run_loop curf cmds stack (Float n)
  | OP op :: cmds, x :: stack, y -> eval_op op x y |> run_loop curf cmds stack 
  | OP _ :: _, _, _ -> runtime_error ()

  | CALL f :: cmds, _, _ -> failwith "not implmented yet"
    (* begin match List.assoc_opt f fs with
    | None      -> runtime_error ()
    | Some code ->
      run_loop fs (Some f) code (RetAddr(curf, cmds) :: stack) acc
    end *)
  | RET :: _, RetAddr(curf, cmds) :: stack, _ -> run_loop curf cmds stack acc

  | RET :: _, _, _ -> runtime_error ()

  (* | READ :: cmds, _, _ ->
    let x = read_line () |> int_of_string in
    run_loop fs curf cmds stack (Num x) *)

  | WRITE :: cmds, _, x ->
    begin match x with
      | Num x -> print_endline (string_of_int x);  run_loop curf cmds stack acc
      | Float f -> print_endline (string_of_float f);  run_loop curf cmds stack acc
      | _ -> runtime_error()
    end 
  | BRANCH(cmds1, cmds2) :: cmds, _, acc -> 
    if (to_float acc) == 0.0 then run_loop curf (cmds2 @ cmds) stack acc
    else run_loop curf (cmds1 @ cmds) stack acc

  | WHILE(cmds1, cmds2) :: cmds, _, _ ->
    let cmds = cmds1 @ BRANCH(cmds2 @ [ WHILE(cmds1, cmds2) ], []) :: cmds in
    run_loop curf cmds stack acc

let run cmds =
  run_loop None cmds [] Undef
