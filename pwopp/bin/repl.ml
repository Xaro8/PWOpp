open Unix
open Zipper
open Cf_monad2

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
      | s -> print_string s;flush Stdlib.stdout; Nothing  
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
  if n > 0  then "\027[G" ^ "\027[" ^ Int.to_string n ^ "A" ^ "\027[0J"  
  else "\027[G\027[0J"
let printf str = print_string str; flush Stdlib.stdout
let erase_all prev = prev |> unzip |> numberoflines |> c_code |> printf

let rec create_string (acc : 'a zip) (hist : 'a zip zip) br = 
  match read_sym() with 
  | Normal x -> 
    erase_all acc;
    let acc = add_prev x acc in 
    print_z acc true;
    if x = '{'      then create_string acc hist (br+1)
    else if x = '}' then create_string acc hist (max (br-1) 0)
    else create_string acc hist br
  | Endl -> 
    erase_all acc;
    let acc = add_prev '\n' acc in 
    print_z acc true;
    let s, b = list_fcrop (acc |> unzip |> List.rev) ';' in 
    let s =  List.rev s in 
    (* print_endline (Bool.to_string b); *)
    if b  then zip s, hist , br
          else create_string (zip s) hist br
  | Up ->   update acc (get_prev acc hist) br
  | Down -> update acc (get_next acc hist) br
  | Backspace -> 
    erase_all acc;
    let acc = (try List.tl (fst acc),snd acc with _ -> acc) in  
    print_z acc true;
    create_string acc hist br 
  | Nothing -> create_string acc hist br
  | Left -> 
    begin match acc with 
      | [], _ -> create_string acc hist br
      | '\n'::prev, next -> 
        Printf.printf "\027[F\027[%nC" (distfromlast prev); 
        flush Stdlib.stdout; 
        create_string (prev,'\n'::next) hist br
      | x::prev, next -> printf "\027[D"; create_string (prev,x::next) hist br
    end
  | Right ->
    begin match acc with 
      | _, [] -> create_string acc hist br
      | prev,'\n'::next -> printf "\027[E"; create_string ('\n'::prev,next) hist br
      | prev, x::next   -> printf "\027[C"; create_string (x::prev,next) hist br
    end 
and update prev v br = 
  erase_all prev;
  match  v with
  | Some acc, hist  ->  print_z acc false; create_string acc hist br
  | None, hist -> create_string ([],[]) hist br

let repl state input  =
  let input = unzip_to_str input in
  if input = "exit;" then exit 0;
  let lexbuf = Lexing.from_string input in
  let stmt = Parser.prog Lexer.token lexbuf in
  let mon = Eval.eval_stmt false stmt >>= fun v ->
    (Eval.print_value v;print_endline("-------------------------"); get_state)
  in run mon state 
 
let rec loop (hist : char zip zip) (br : int) prompt (state:state) : unit = 
  let str, _, br = create_string prompt hist br in 
  if br = 0 then 
    let str = List.tl (fst str), snd(str) in 
    repl state str |> loop (add_prev str hist) 0 ([],[])
  else loop hist br str state
 
let run () =
  set_raw_mode true; 
  print_endline "Hello from pwopp REPL! :)\n------------------------- ";
  try 
    loop ([], []) 0 ([],[]) M.empty; 
    set_raw_mode false 
  with _ -> set_raw_mode false