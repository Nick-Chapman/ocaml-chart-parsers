
module Rule : module type of Rule

module Res : sig
  type 'a t =
  | Succ of 'a
  | Amb of [`number_parses of int] * 'a list
  | Fail of [`position of int]
      [@@deriving sexp_of]
end

val parse
  :  dict: ('tok -> 'a list)
  -> gram : ('a,'a) Rule.t
  -> accept: ('a -> 'res option)
  -> words: 'tok list
  -> unit
  -> 'res Res.t
