
module Rule : sig
    
  (* The type of chart-parse rules,
     reading symbols of type 'sym, producing values of type 'a *)
  type ('sym,'a) t

  (* primitive constructors *)
  val get    : ('sym,'sym) t
  val fail   : ('sym,'a) t
  val return : 'a -> ('sym,'a) t
  val alt    : ('sym,'a) t list -> ('sym,'a) t
  val guard  : ('sym, 'a option) t -> ('sym,'a) t
  val map    : ('sym,'a) t -> ('a -> 'b) -> ('sym,'b) t
  val bind   : ('sym,'a) t -> ('a -> ('sym,'b) t) -> ('sym,'b) t
  val seq    : ('sym,'a) t -> ('sym,'b) t -> ('sym, 'a * 'b) t

  (* useful combination of guard,map,get *)
  val select : ('sym -> 'a option) -> ('sym,'a) t

  (* infixes for map,bind,seq *)
  val (>>|)  : ('sym,'a) t -> ('a -> 'b) -> ('sym,'b) t
  val (>>=)  : ('sym,'a) t -> ('a -> ('sym,'b) t) -> ('sym,'b) t
  val ($$)   : ('sym,'a) t -> ('sym,'b) t -> ('sym, 'a * 'b) t

  (* more infixes for sequencing.
     [-$$] ignores the value carried by its left argument
     [@>] is applicative style sequencing *)
  val (-$$)  : ('sym,unit) t -> ('sym,'b) t  -> ('sym,'b) t
  val (@>)   : ('sym,'a) t -> ('sym,'a -> 'b) t -> ('sym,'b) t

  (* changing the symbol type *)  
  val embed  : ('sym2 -> 'sym option) -> ('sym,'a) t -> ('sym2,'a) t

  (* more combinators *)
  val sequence : ('sym,'a) t list -> ('sym,'a list) t
    
end
  
module Res : sig
  (** [Chart.Res.t] is the result of parsing.
      For convenience, the non-ambiguous parse case is distinguished*)
  type 'a t =
  | Succ of 'a
  | Amb of [`number_parses of int] * 'a list
  | Fail of [`position of int]
  [@@deriving sexp_of]
end

(** [parse ~dict ~gram ~accept ~tokens ()]

    The parse function is polymorphic over the types for token, symbol, and result.

    [tokens] is the list of tokens to parse.
    [gram] provides the ('sym,'sym) rule.
    [dict] provides the set of symbols asociated with a given token.
    [accept] identifies the start symbol, and extract a result.
*)
val parse
  :  dict: ('token -> 'sym list)
  -> gram : ('sym,'sym) Rule.t
  -> accept: ('sym -> 'res option)
  -> tokens: 'token list
  -> unit
  -> 'res Res.t
