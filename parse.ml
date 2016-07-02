open Core.Std
open Rule_type

type pos = int
type 'a k = pos -> 'a -> unit

module Pipe : sig

  type 'a t
  val create : unit -> 'a t
  val register_callback : 'a t -> 'a k -> unit
  val supply : 'a t -> pos -> 'a -> unit

  val trace : tag:string -> 'a t -> unit

  val size : 'a t -> int * int
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

  let trace ~tag t =
    printf "%s: #callbacks = %d, #items = %d\n"
      tag
      (List.length t.callbacks)
      (List.length t.items)

  let size t = 
    (List.length t.callbacks),
    (List.length t.items)
      
end

type 'a t = {
  gram : ('a,'a) Rule.t;
  pipes : (pos, 'a Pipe.t) Hashtbl.t;
  debug_opt : (origin:pos -> 'a k) option;
}

let _trace t =
  let cmp (p1,_) (p2,_) = Int.compare p1 p2 in
  List.iter (List.sort ~cmp (Hashtbl.to_alist t.pipes)) ~f:(fun (pos,pipe) ->
    Pipe.trace ~tag:(sprintf "%d" pos) pipe;
  )

let trace t =
  let (c,i) = 
    List.fold ~init:(0,0) (Hashtbl.to_alist t.pipes) ~f:(fun (c,i) (_,pipe) ->
      let (c2,i2) = Pipe.size pipe in
      (c+c2,i+i2)
    )
  in eprintf "(c,i)=(%d,%d)\n" c i
    
let create ~gram ?debug () =
  let pipes : (pos, 'a Pipe.t) Hashtbl.t = Hashtbl.Poly.create () in
  { gram; pipes; debug_opt = debug }

let want (type n) ({gram;pipes;debug_opt} : n t) =

  let rec get_pipe origin =
    match Hashtbl.find pipes origin with
    | Some pipe -> pipe
    | None ->
      let pipe = Pipe.create () in
      begin match debug_opt with | None -> () | Some debug ->
	Pipe.register_callback pipe (debug ~origin);
      end;    
      Hashtbl.add_exn pipes ~key:origin ~data:pipe;
      process origin gram (Pipe.supply pipe);
      pipe

  and want ~origin k =
    let pipe = get_pipe origin in
    Pipe.register_callback pipe k;

  and process : type a. pos -> (n,a) Rule.t -> a k -> unit =
    fun pos rule k ->
      match rule with
      | Get -> want ~origin:pos k
      | Fail -> ()
      | Return a -> k pos a
      | Alt ts -> List.iter ts ~f:(fun t -> process pos t k)
      | Seq (t1,t2) ->
	process pos t1 (fun pos a ->
	  process pos t2 (fun pos b ->
	    k pos (a,b)))
      | Guard (_name,t) ->
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
