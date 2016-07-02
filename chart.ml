open Core.Std

module Rule = Rule  

module Res = struct
  type 'a t =
    | Succ of 'a
    | Amb of [`number_parses of int] * 'a list
    | Fail of [`position of int]
  [@@deriving sexp_of]
end

let show trace tag t =
  if trace then (
    printf "---------- %s ----------\n" tag;
    Parse.trace t;
  )
  
let parse1 ?(trace=false) ~dict ~gram ~accept ~words ?debug () =
  let t = Parse.create ~gram ?debug () in
  let sentence_length = List.length words in
  let results : 'n list ref = ref [] in
  let () =
    Parse.want t ~origin:0 (fun pos sym ->
      if pos = sentence_length then (
        match accept sym with
        | Some res -> results := res :: !results
        | None -> ()
      ))
  in
  (*show trace "start" t;*)
  let rec supply ~pos words =
    match Parse.supply t pos with
    | None -> Res.Fail (`position pos)
    | Some supply_sym ->
      match words with
      | word::words ->
	List.iter (dict word) ~f:supply_sym;
	(*show trace (sprintf "after-pos:%d" pos) t;*)
	supply ~pos:(pos+1) words
      | [] ->
	match !results with
	| [] -> Res.Fail (`position sentence_length)
	| [x] -> Res.Succ x
	| _::_::_ as xs -> Res.Amb (`number_parses (List.length xs), xs)
  in
  let res = supply ~pos:0 words in
  show trace "end-of-parse" t;
  res

  
(*type ('tok,'a) or_word = Word of 'tok | Sym of 'a 

let extend_with_dictionary_lookup ~dict ~gram =
  let open Rule in
  alt [
    (get >>= fun _ -> fail);
    (get >>= function Sym _ -> fail | Word s -> alt (List.map (dict s) ~f:return));
    (embed "extend_with_dictionary_lookup" (function Sym x -> Some x | Word _ -> None) gram)
  ] >>| fun x -> Sym x

let parse2d ?debug () ~dict ~gram ~accept ~words =
  parse1
    ~dict:(fun x -> [x])
    ~gram:(extend_with_dictionary_lookup ~dict ~gram)
    ~accept:(function Sym x -> accept x | Word _ -> None)
    ~words:(List.map words ~f:(fun x -> Word x))
    ?debug
    ()
*)

let parse2 ?trace ~dict ~gram ~accept ~words =
  parse1
    ?trace
    ~dict
    ~gram
    ~accept
    ~words
    ()

