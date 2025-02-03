open Unix
open Zipper
open Cf_monad2
let disp = ref "" 
type inp = 
  | Normal of char
  | Up | Down | Left | Right
  | Endl
  | Backspace
  | Nothing
let set_raw_mode enable =
  let termio = tcgetattr stdin in
  let new_termio =
    if enable then
      { termio with c_icanon = false; c_echo = false} 
    else
      { termio with c_icanon = true; c_echo = true }
  in
  tcsetattr stdin TCSAFLUSH new_termio
let read_sym () : inp =
  let buf = Bytes.create 3 in
  let _ = read stdin buf 0 1 in
  match Bytes.get buf 0 with
  | '\027' ->  (* ESC *)
      let _ = read stdin buf 1 2 in
      begin match Bytes.to_string buf with
      | "\027[A" -> Up
      | "\027[B" -> Down
      | "\027[C" -> Right
      | "\027[D" -> Left
      | _ -> Nothing  
      end
  | '\x7F' -> Backspace  
  | '\n'   -> Endl 
  | ch     -> Normal ch
let rec list_fcrop xs v =
  match xs with 
    | [] -> [], false
    | x:: _ when x = v -> xs, true
    | '\n'::xs -> 
      let l, b = list_fcrop xs v in '\n'::l, b 
    | x::xs -> 
      let l, b = list_fcrop xs v in 
      if b then l, b
      else x::l, b 
      
let string_of_list li = li |> List.to_seq |> String.of_seq 
let c_code n = 
  if n > 0  then "\027[" ^ Int.to_string n ^ "F" ^ "\027[0J"  
  else "\027[G\027[0J"
let printf str = print_string str; flush Stdlib.stdout
let erase_all prev = prev |> fst |> numberoflines |> c_code |> printf

let get_terminal_width () =
  let ic = Unix.open_process_in "tput cols" in
  try
    let width = input_line ic |> int_of_string in
    close_in ic;
    width
  with _ ->
    close_in ic;
    40

let rec create_string (acc : 'a zip) (hist : 'a zip zip) = 
  match read_sym() with 
  | Normal x -> 
    erase_all acc;
    let acc = add_prev x acc in 
    print_z acc true;
    if x = '{'      then create_string acc hist 
    else if x = '}' then create_string acc hist 
    else create_string acc hist
  | Endl -> 
    erase_all acc;
    let acc = add_prev '\n' acc in 
    print_z acc true;
    let s, b = list_fcrop (acc |> unzip |> List.rev) ';' in 
    let s =  List.rev s in 
    (* print_endline (Bool.to_string b); *)
    if b  then zip s, hist
          else create_string (zip s) hist 
  | Up ->   update acc (get_prev acc hist)
  | Down -> update acc (get_next acc hist)
  | Backspace -> 
    erase_all acc;
    let acc = (try List.tl (fst acc),snd acc with _ -> acc) in  
    print_z acc true;
    create_string acc hist 
  | Nothing -> create_string acc hist
  | Left -> 
    begin match acc with 
      | [], _ -> create_string acc hist
      | x::prev, next ->
        erase_all acc;
        let acc = (prev,x::next) in 
        print_z acc true;
        create_string acc hist
    end
  | Right ->
    begin match acc with 
      | _, [] -> create_string acc hist
      | prev, x::next   -> 
        erase_all acc;
        let acc = (x::prev,next) in 
        print_z acc true;
        create_string acc hist
    end 
and update prev v = 
  erase_all prev;
  match  v with
  | Some acc, hist  ->  print_z acc false; create_string acc hist 
  | None, hist -> create_string ([],[]) hist 

let repl state input  =
  let input = unzip_to_str input in
  if input = "exit;" then exit 0;
  let lexbuf = Lexing.from_string input in
  let stmt = Parser.prog Lexer.token lexbuf in
  let mon = Eval.eval_stmt false stmt >>= fun v ->
    ( Eval.print_value v;
      print_endline(!disp); 
      get_state)
  in run mon state 

let closed zip : bool =
  let help acc a = 
    if a = '{' then acc + 1 
    else if a = '}' then acc - 1 
    else acc
  in (List.fold_left help 0 (unzip zip)) = 0
  
let rec loop (hist : char zip zip) prompt (state:state) : unit = 
  let str, _ = create_string prompt hist in 
  if closed(str) then 
    let str = List.tl (fst str), snd(str) in 
    repl state str |> loop (add_prev str hist) ([],[])
  else loop hist str state


let run () =
  set_raw_mode true; 
  let wd = get_terminal_width() in 
  disp := String.make wd '-';
  let tpr = "Hello from pwopp REPL! :)" in 
  print_endline !disp; 
  Printf.printf "\027[%nC" ((wd - String.length tpr)/2);
  print_endline (tpr);
  print_endline !disp; 
  try 
    loop ([], []) ([],[]) M.empty; 
    set_raw_mode false 
  with _ -> set_raw_mode false