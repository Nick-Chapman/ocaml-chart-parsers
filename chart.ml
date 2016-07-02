open Core.Std

module Rule = Rule  

module Res = struct
  type 'a t =
    | Succ of 'a
    | Amb of [`number_parses of int] * 'a list
    | Fail of [`position of int]
  [@@deriving sexp_of]
end
  
let parse ~dict ~gram ~accept ~words () =
  let t = Parse.create ~gram () in
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
  let rec supply ~pos words =
    match Parse.supply t pos with
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
