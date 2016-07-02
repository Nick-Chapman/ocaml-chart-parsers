open Core.Std

open Chart.Rule

let number_of_parses = Chart.Res.(function  
  | Succ _ -> Some 1
  | Amb (`number_parses n,_) -> Some n
  | Fail _ -> None)

type nt = S | Q | Word of string

let dict : string -> nt list =
  fun word ->
    [Word word]

let accept : nt -> unit option =
  function
  | S -> Some ()
  | _ -> None

type 'a rule = (nt,'a) Chart.Rule.t

let x : unit rule = select (function Word _ -> Some () | _ -> None)
let q : unit rule = select (function Q -> Some () | _ -> None)

let run ~gram s =
  let words = List.map (String.to_list s) ~f:(String.make 1) in
  printf "words: %s\n" (String.concat ~sep:" " words);
  let res = number_of_parses (Chart.parse ~dict ~gram ~accept ~words ()) in
  printf !"res: %{sexp:int option}\n" res

module Catalan = struct
    
  let gram : nt rule = alt [
    (q -$$ return S);
    (x -$$ x -$$ return Q);
    (q -$$ x -$$ return Q);
    (x -$$ q -$$ return Q);
    (q -$$ q -$$ return Q);
  ]
    
  let%expect_test _ =
    run ~gram "123456";
    [%expect {|
	words: 1 2 3 4 5 6
	res: (42)|}]
end

module ChooseFrom7 = struct
    
  let gram : nt rule = alt [
    (q -$$ q -$$ q -$$ q -$$ q -$$ q -$$ q -$$ return S);
    (x -$$ return Q);
    (return Q);
  ]

  let%expect_test _ =
    run ~gram "123"; (* choose 3 from 7 *)
    [%expect {|
	words: 1 2 3
	res: (35)|}]
end
  
