
(* Core chart parsing algorithm *)

type 'a t

val trace : 'a t -> unit

type pos = int

val create :
  gram: ('a,'a) Rule.t
  -> ?debug:(origin:pos -> pos -> 'a -> unit)
  -> unit
  -> 'a t

val want :
  'a t
  -> origin:pos
  -> (pos -> 'a -> unit)
  -> unit

val supply : 'a t -> pos -> ('a -> unit) option
