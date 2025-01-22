open Ast

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

type 'a result =
  | Normal of 'a
  | Return of 'a
  | BreakSignal
  | ContinueSignal

module CFmonad (State : sig type t end) : sig 
  include Monad
  (* val signal_return : 'a -> 'a t *)
  val signal_break : 'a t
  val signal_continue : 'a t
  val catch_return  : 'a t -> (unit -> 'a t) -> 'a t
  (* val catch_break  : 'a t -> (unit -> 'a t) -> 'a t
  val catch_continue  : 'a t -> (unit -> 'a t) -> 'a t *)
  val get : State.t t
  val set : State.t -> unit t
  val run : State.t -> 'a t -> 'a result 
end = struct
  type 'r ans =  State.t -> 'r result * State.t
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans }
  let return a = { run = fun k -> k a}
  let bind m f = { run = fun k ->
    let cont x = (f x).run k in
    m.run cont  
  }

  let signal_continue = {run = fun _ state-> (ContinueSignal, state)}
  let signal_break = {run = fun _ state-> (BreakSignal, state)}
  let catch_return (m:'a t) (f : unit -> 'a t) ={ run = fun k state -> 
    match m.run k state with
    | Return v, state -> k v state
    | a -> a 
  }
  let get = {run = fun k s -> k s s} 
  let set state = {run = fun k _ -> k () state}
  let run init_state m = fst (m.run (fun x -> (fun s -> (Normal x, s))) init_state)
end