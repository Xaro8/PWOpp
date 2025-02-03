type 'a zip = 'a list * 'a list 

let go_prev = function
  | x::prev, next -> prev, x::next
  | [], next -> [],next 
let go_next = function
  | prev, x::next -> x::prev, next
  | prev, [] -> prev,[]
let add_prev el hist = (hist |> fst |> List.cons el), snd hist
let add_next el hist = fst hist, hist |> snd |> List.cons el
let unzip zip = (List.rev (fst zip)) @ snd zip
let zip str = List.rev str, [] 
let unzip_to_str zip = zip |> unzip |> List.to_seq |> String.of_seq 

let get_prev (el :'a zip) (hist: 'a zip zip) = 
  let el = unzip el in 
  match hist with
  | x::prev, next -> if el <> [] && el <> ['\n'] then Some x, (prev, (zip el)::next) else Some x, (prev, next)
  | [], next      -> if el <> [] && el <> ['\n'] then None  , ([],   (zip el)::next) else None,   ([],   next)
let get_next (el :'a zip) (hist: 'a zip zip) = 
  let el = unzip el in 
    match hist with
    | prev, x::next ->if el <> [] && el <> ['\n'] then Some x, ((zip el)::prev, next) else Some x, (prev, next)
    | prev, [] -> if el <> [] && el <> ['\n'] then None, ((zip el)::prev,[])  else None, (prev,[])
let numberoflines st : int =
  List.fold_left (fun acc a -> if a = '\n' then acc + 1 else acc) 0 st
let rec distfromlast st = 
  match st with 
  | [] -> 0
  | '\n' :: _ -> 0
  | _ :: st -> 1 + distfromlast st
let print_t acc = 
  let tpr = acc |> snd |> List.to_seq |> String.of_seq  in
  print_string ("\027[s"^tpr^"\027[u"); flush stdout
    
let print_z acc preserve= 
  if preserve then begin
      acc |> fst |> List.rev |> List.to_seq |> String.of_seq |> print_string ; 
      print_t acc
    end
  else  acc |> unzip_to_str |> print_string; flush stdout 