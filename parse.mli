
(* Core chart parsing algorithm *)

type 'a t

type pos = int

val create :
  gram: ('a,'a) Rule.t
  -> unit
  -> 'a t

val want :
  'a t
  -> origin:pos
  -> (pos -> 'a -> unit)
  -> unit

val supply : 'a t -> pos -> ('a -> unit) option
