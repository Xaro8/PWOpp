open Ast
exception No_handler_found 
type signal =
  | SBreak 
  | SContinue
  | SReturn of value

module M = Map.Make(String)
type state = value M.t
type 'a t = { run :'r. ('a -> 'r ans) -> 'r ans }
and 'a ans = state -> 'a hstack -> 'a
and 'a hstack = {signal : signal -> 'a ans}
let signal st sg = (st.signal sg) 

let catch_break m h k state st  = 
  m.run (fun x state' _-> k x state' st) state { signal = fun sg -> 
    match sg with 
    | SBreak -> (h ()).run k 
    | _ -> st.signal sg
  }
let catch_breakm (m : 'a t) (h : unit -> 'b t)  : 'b t = {run = fun k state st -> catch_break m h k state st}

let catch_continue m h k  state st  = 
m.run (fun x state' _ -> k x  state' st) state { signal = fun sg -> 
  match sg with 
  | SContinue -> (h ()).run k
  | _ -> st.signal sg
}
let catch_continuem (m : 'a t) (h : unit -> 'b t)  : 'b t = {run = fun k state st -> catch_continue m h k state st}

let catch_return m h k state st = 
  m.run (fun x state' _ -> k x state' st) state { signal = fun sg ->
    match sg with
    | SReturn v -> (h v).run k
    | _ -> st.signal sg
  }
let catch_returnm (m : 'a t) (h : 'a -> 'a t)  : 'a t = {run = fun k state st -> catch_return m h k state st}
let bind (m: 'a t)  (f : 'a -> 'b t) : 'b t = {run = fun k -> 
  let cont x = (f x).run k in m.run cont}

let (>>=) = bind

let return (a : 'a) : 'a t = {run = fun k -> k a}
let get_state : state t = {run = fun k s -> k s s}  
let set_state (nst: state) : value t =  {run = fun k _ -> k VNone nst} 

let run (m: 'a t) initial_state = m.run (fun x _ _-> x ) initial_state {signal = fun _ -> raise No_handler_found}