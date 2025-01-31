type 'a zip = 'a list * 'a list 

let go_prev = function
  | x::prev, next -> prev, x::next
  | [], next -> [],next 
let go_next = function
  | prev, x::next -> x::prev, next
  | prev, [] -> prev,[]
let add_prev el hist = hist |> fst |> List.cons el, snd hist
let add_next el hist = fst hist, hist |> snd |> List.cons el
let unzip zip = (List.rev (fst zip)) @ snd zip
let zip str = List.rev str, [] 
let unzip_to_str zip = zip |> unzip |> List.to_seq |> String.of_seq 

let get_prev (el :'a zip) (hist: 'a zip zip) = 
  let el = unzip el in 
  match hist with
  | x::prev, next ->if el <> [] && el <> ['\n'] then Some x, (prev, (zip el)::next) else Some x, (prev, next)
  | [], next -> if el <> [] && el <> ['\n'] then None, ([],(zip el)::next)  else None, ([],next)
let get_next (el :'a zip) (hist: 'a zip zip) = 
  let el = unzip el in 
    match hist with
    | prev, x::next ->if el <> [] && el <> ['\n'] then Some x, ((zip el)::prev, next) else Some x, (prev, next)
    | prev, [] -> if el <> [] && el <> ['\n'] then None, ((zip el)::prev,[])  else None, (prev,[])