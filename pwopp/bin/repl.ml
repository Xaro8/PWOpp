open Unix
open Zipper

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
      | s -> print_string s; flush Stdlib.stdout; Nothing  
      end
  | '\x7F' -> Backspace  
  | ch ->
      print_char ch;
      flush Stdlib.stdout;
      if Char.equal ch '\n' then Endl else Normal ch
let rec list_fcrop xs v =
  match xs with 
    | [] -> [], false
    | x::_ when x = v -> [x], true
    | x::xs -> let l, b = list_fcrop xs v in x::l, b
let numberoflines st : int =
    List.fold_left (fun acc a-> if a = '\n' then acc + 1 else acc) 0 st
let rec distfromlast st = 
  match st with 
  | [] -> 0
  | '\n' :: _ -> 0
  | _ :: st -> 1 + distfromlast st
let string_of_list li = li |> List.rev |> List.to_seq |> String.of_seq 
let c_code n = 
  if n > 0  then "\027[G" ^ "\027[" ^ Int.to_string n ^ "A" ^ "\027[0J"  
  else "\027[G\027[0J"
let gen_code prev =  prev |> unzip |> numberoflines |> c_code 
let printf str = print_string str; flush Stdlib.stdout

let rec create_string (acc : 'a zip) (hist : 'a zip zip) = 
  match read_sym() with 
  | Normal x -> 
    printf "\027[0J"; 
    let tpr = acc |> snd |> List.rev |> string_of_list in
    let n = String.length tpr in 
    print_string tpr; 
    if n > 0 then Printf.printf "\027[%nD" n;
    flush Stdlib.stdout; 
    create_string (add_prev x acc ) hist
  | Endl -> let s,b = list_fcrop (unzip acc) ';' in 
    if b then zip s, hist else create_string (zip(s@['\n'])) hist 
  | Up ->   update acc (get_prev acc hist)
  | Down -> update acc (get_next acc hist)
  | Backspace -> printf "\b \b"; create_string (try List.tl (fst acc),snd acc with _ -> acc) hist
  | Nothing -> create_string acc hist
  | Left -> 
    begin match acc with 
      | [],_ -> create_string acc hist
      | '\n'::prev,next -> Printf.printf "\027[F\027[%nC" (distfromlast prev); flush Stdlib.stdout; create_string (prev,'\n'::next) hist
      | x::prev,next -> printf "\027[D"; create_string (prev,x::next) hist
    end
  | Right ->
    begin match acc with 
      | _,[] -> create_string acc hist
      | prev,'\n'::next -> printf "\027[E"; create_string ('\n'::prev,next) hist
      | prev,x::next -> printf "\027[C"; create_string (x::prev,next) hist
    end 
and update prev v = 
  prev |> gen_code |> printf; 
  match  v with
  | Some acc, hist  ->  
    acc |> unzip_to_str |> printf;
    create_string acc hist
  | None, hist -> create_string ([],[]) hist
let rec loop hist = 
  let str, _ = create_string ([],[]) hist in 
  let hist = add_prev str hist in 
  let str = unzip_to_str str in
  (* let str = "\n" ^ str in 
  print_endline str;  *)
  if String.equal str "exit;" then () else loop hist
let run () =
  set_raw_mode true; 
  try 
    loop ([],[]);
    set_raw_mode false 
  with _ -> set_raw_mode false