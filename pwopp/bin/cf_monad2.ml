open Ast

type 'a signal =
  | SBreak 
  | SContinue
  | SReturn of 'a 

type 'a t =  ('a -> 'a ans) -> 'a ans
and 'a ans = 'a hstack -> unit
and 'a hstack = {signal : 'a signal -> 'a ans}
and frame = 
  | HBreak : (unit -> 'a t) * ('a -> 'a ans) -> frame
  | HContiniue : (unit -> 'a t) * ('a -> 'a ans) -> frame 
  | HReturn: ('a -> 'a t) * ('a -> 'a ans) -> frame


  