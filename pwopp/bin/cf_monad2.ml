open Ast
exception No_handler_found 
type 'a signal =
  | SBreak 
  | SContinue
  | SReturn of 'a 

module M = Map.Make(String)
type state = value M.t
type 'a t = { run :'r. ('a -> 'r ans) -> 'r ans }
and 'a ans = state -> 'a hstack -> 'a
and 'a hstack = {signal : 'a signal -> 'a ans}
and frame = 
  | HBreak : (unit -> 'a t) * ('a -> 'a ans) -> frame
  | HContiniue : (unit -> 'a t) * ('a -> 'a ans) -> frame 
  | HReturn: ('a -> 'a t) * ('a -> 'a ans) -> frame


let signal (st: 'a hstack) (sg: 'a signal) = st.signal sg

let catch_break (m: 'a t) (h: unit -> 'a t) (k : 'a -> 'a ans) (state : state) (st : 'a hstack)  = 
  m.run (fun x state' _-> k x state' st) state { signal = fun sg -> 
    match sg with 
    | SBreak -> (h ()).run k
    | _ -> st.signal sg
  }

let catch_continue (m: 'a t) (h: unit -> 'a t) (k : 'a -> 'a ans) (state: state) (st : 'a hstack)  = 
m.run (fun x state' _ -> k x  state' st) state { signal = fun sg -> 
  match sg with 
  | SContinue -> (h ()).run k
  | _ -> st.signal sg
}

let catch_return (m: 'a t) (h: 'a -> 'a t) (k : 'a -> 'a ans) (state: state) (st : 'a hstack)  = 
m.run (fun x state' _ -> k x state' st) state { signal = fun sg -> 
  match sg with 
  | SReturn v -> (h v).run k
  | _ -> st.signal sg
}
let bind (m: 'a t)  (f : 'a -> 'b t) : 'b t = {run = fun k -> 
  let cont x = (f x).run k in m.run cont}

let (>>=) = bind
let return (a : 'a) : 'a t = {run = fun k -> k a}
let get_state : state t = {run = fun k s -> k s s}  
let set_state (nst: state) : value t =  {run = fun k _ -> k VNone nst} 

let run (m: 'a t) initial_state = m.run (fun x _ _-> x ) initial_state {signal = fun _ -> raise No_handler_found}