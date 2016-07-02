
(* The type of chart-parse rules,
   reading symbols of type 'n, producing values of type 'a *)
type ('n,'a) t = ('n,'a) Rule_type.t

(* primitive constructors *)
val get    : ('n,'n) t
val fail   : ('n,'a) t
val return : 'a -> ('n,'a) t
val alt    : ('n,'a) t list -> ('n,'a) t
val guard  : ('n, 'a option) t -> ('n,'a) t
val map    : ('n,'a) t -> ('a -> 'b) -> ('n,'b) t
val bind   : ('n,'a) t -> ('a -> ('n,'b) t) -> ('n,'b) t
val seq    : ('n,'a) t -> ('n,'b) t -> ('n, 'a * 'b) t

(* useful combination of guard,map,get *)
val select : ('n -> 'a option) -> ('n,'a) t

(* infixes for map,bin,seq *)
val (>>|)  : ('n,'a) t -> ('a -> 'b) -> ('n,'b) t
val (>>=)  : ('n,'a) t -> ('a -> ('n,'b) t) -> ('n,'b) t
val ($$)   : ('n,'a) t -> ('n,'b) t -> ('n, 'a * 'b) t

(* more infixes for sequencing *)
val (-$$)  : ('nt,unit) t -> ('nt,'b) t  -> ('nt,'b) t
val (@>)   : ('nt,'a) t -> ('nt,'a -> 'b) t -> ('nt,'b) t

(* changing the symbol type *)  
val embed  : ('n2 -> 'n option) -> ('n,'a) t -> ('n2,'a) t
