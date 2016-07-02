open Core.Std
include Rule_type

let get = Get
let fail = Fail
let return a = Return a
let alt ts = Alt ts
let guard name t = Guard (name,t)
let map t f = Map (t,f)
let bind t f = Bind (t,f)
let seq t1 t2 = Seq (t1,t2)

let select name f = guard name (map get f)

let (>>|) = map
let (>>=) = bind
let ($$)  = seq
  
let (-$$) t1 t2 = seq t1 t2 >>| fun ((),b) -> b
let (@>)  t1 t2 = seq t1 t2 >>| fun (a,f) -> f a

let embed (type n) (type n2) name (e:n2 -> n option) =    
  let rec embed : type a. (n,a)t -> (n2,a)t = function
    | Get -> Guard (name,Map (Get,e))
    | Fail -> Fail
    | Return a -> Return a
    | Alt ts -> Alt (List.map ts ~f:embed)
    | Seq (t1,t2) -> Seq (embed t1, embed t2)
    | Guard (name,t) -> Guard (name,embed t)
    | Map (t,f) -> Map (embed t,f)
    | Bind (t,f) -> Bind (embed t, fun n -> embed (f n))
  in embed
  
