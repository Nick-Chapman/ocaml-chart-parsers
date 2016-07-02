
(* The representation type for chart-parse rules *)

type (_,_) t =
| Get        :                                     ('n,'n) t
| Fail       :                                     ('n,'a) t
| Return     : 'a                               -> ('n,'a) t
| Alt        : ('n,'a) t list                   -> ('n,'a) t
| Guard      : ('n, 'a option) t                -> ('n,'a) t
| Map        : ('n,'a) t * ('a -> 'b)           -> ('n,'b) t
| Bind       : ('n,'a) t * ('a -> ('n,'b) t)    -> ('n,'b) t
| Seq        : ('n,'a) t * ('n,'b) t            -> ('n,'a * 'b) t
