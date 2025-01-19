open Ast

type result =
  | Normal of value
  | Return of value
  | BreakSignal
  | ContinueSignal

module EvalMonad = struct
  module M = Map.Make(String)
  type state = value M.t
  type 'a t = state -> result * state

  let return x : 'a t = fun state -> (Normal x, state)
  let bind (m :'a t) (f : value -> 'b t) : 'b t = fun state ->
    match m state with
    | (Normal x, state') -> f x state'
    | (Return v, _) -> (Return v, state)
    | (BreakSignal, _) -> (BreakSignal, state)
    | (ContinueSignal, _) -> (ContinueSignal, state)
  let (>>=) = bind
  let get_state = fun state -> (Normal state, state)
  let set_state new_state = fun _ -> (Normal VNone, new_state)

  let signal_return v = fun state -> (Return v, state)
  let signal_break = fun state -> (BreakSignal, state)
  let signal_continue = fun state -> (ContinueSignal, state)
end