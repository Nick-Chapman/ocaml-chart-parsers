open Core.Std

module Rule = struct

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

  let get = Get
  let fail = Fail
  let return a = Return a
  let alt ts = Alt ts
  let guard t = Guard t
  let map t f = Map (t,f)
  let bind t f = Bind (t,f)
  let seq t1 t2 = Seq (t1,t2)

  let select f = guard (map get f)

  let (>>|) = map
  let (>>=) = bind
  let ($$)  = seq
    
  let (-$$) t1 t2 = seq t1 t2 >>| fun ((),b) -> b
  let (@>)  t1 t2 = seq t1 t2 >>| fun (a,f) -> f a

  let embed (type n) (type n2) (e:n2 -> n option) =    
    let rec embed : type a. (n,a)t -> (n2,a)t = function
      | Get -> Guard (Map (Get,e))
      | Fail -> Fail
      | Return a -> Return a
      | Alt ts -> Alt (List.map ts ~f:embed)
      | Seq (t1,t2) -> Seq (embed t1, embed t2)
      | Guard t -> Guard (embed t)
      | Map (t,f) -> Map (embed t,f)
      | Bind (t,f) -> Bind (embed t, fun n -> embed (f n))
    in embed

  let rec sequence : ('s,'a) t list -> ('s,'a list) t = function
    | [] -> return []
    | x::xs -> x @> sequence xs @> return (fun xs x -> x::xs)
    
end

type pos = int
type 'a k = pos -> 'a -> unit
  
module Pipe : sig

  type 'a t
  val create : unit -> 'a t
  val register_callback : 'a t -> 'a k -> unit
  val supply : 'a t -> pos -> 'a -> unit

end = struct

  type 'a t = {
    mutable items : (pos * 'a) list;
    mutable callbacks : 'a k list;
  }

  let create () = {
    items = [];
    callbacks = [];
  }

  let register_callback t k =
    t.callbacks <- k :: t.callbacks;
    List.iter (List.rev t.items) ~f:(fun (pos,a) -> k pos a)

  let supply t pos a =
    t.items <- (pos,a) :: t.items;
    List.iter (List.rev t.callbacks) ~f:(fun k -> k pos a)
      
end

module State : sig

  type 'a t

  val create : gram: ('a,'a) Rule.t -> unit -> 'a t

  val want : 'a t -> origin:pos -> (pos -> 'a -> unit) -> unit

  val supply : 'a t -> pos -> ('a -> unit) option

end = struct

  type 'a t = {
    gram : ('a,'a) Rule.t;
    pipes : (pos, 'a Pipe.t) Hashtbl.t;
  }

  let create ~gram () =
    let pipes : (pos, 'a Pipe.t) Hashtbl.t = Hashtbl.Poly.create () in
    { gram; pipes }

  let want (type n) ({gram;pipes} : n t) =

    let rec get_pipe origin =
      match Hashtbl.find pipes origin with
      | Some pipe -> pipe
      | None ->
	let pipe = Pipe.create () in
	Hashtbl.add_exn pipes ~key:origin ~data:pipe;
	process origin gram (Pipe.supply pipe);
	pipe

    and want ~origin k =
      let pipe = get_pipe origin in
      Pipe.register_callback pipe k;

    and process : type a. pos -> (n,a) Rule.t -> a k -> unit =
      fun pos rule k ->
	let open Rule in
	match rule with
	| Get -> want ~origin:pos k
	| Fail -> ()
	| Return a -> k pos a
	| Alt ts -> List.iter ts ~f:(fun t -> process pos t k)
	| Seq (t1,t2) ->
	  process pos t1 (fun pos a ->
	    process pos t2 (fun pos b ->
	      k pos (a,b)))
	| Guard t ->
	  process pos t (fun pos opt ->
	    match opt with
	    | None -> ()
	    | Some a -> k pos a)
	| Map (t,f) ->
	  process pos t (fun pos a ->
	    k pos (f a))
	| Bind (t,f) ->
	  process pos t (fun pos a ->
	    process pos (f a) k)
    in
    want
      
  let supply t pos =
    match Hashtbl.find t.pipes pos with
    | Some pipe -> Some (Pipe.supply pipe (pos+1))
    | None -> None
      
end
  
  
module Res = struct
  type 'a t =
    | Succ of 'a
    | Amb of [`number_parses of int] * 'a list
    | Fail of [`position of int]
  [@@deriving sexp_of]
end
  
let parse ~dict ~gram ~accept ~words () =
  let t = State.create ~gram () in
  let sentence_length = List.length words in
  let results : 'n list ref = ref [] in
  let () =
    State.want t ~origin:0 (fun pos sym ->
      if pos = sentence_length then (
        match accept sym with
        | Some res -> results := res :: !results
        | None -> ()
      ))
  in
  let rec supply ~pos words =
    match State.supply t pos with
    | None -> Res.Fail (`position pos)
    | Some supply_sym ->
      match words with
      | word::words ->
	List.iter (dict word) ~f:supply_sym;
	supply ~pos:(pos+1) words
      | [] ->
	match !results with
	| [] -> Res.Fail (`position sentence_length)
	| [x] -> Res.Succ x
	| _::_::_ as xs -> Res.Amb (`number_parses (List.length xs), xs)
  in
  supply ~pos:0 words
