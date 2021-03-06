open Core.Std
open Chart.Rule

let number_of_parses = Chart.Res.(function  
  | Succ _ -> Some 1
  | Amb (`number_parses n,_) -> Some n
  | Fail _ -> None)

type nt = S | Q | Token of string

type 'a rule = (nt,'a) Chart.Rule.t

let x : unit rule = select (function Token _ -> Some () | _ -> None)
let q : unit rule = select (function Q -> Some () | _ -> None)

(* very ambiguous grammar: all possible binary bracketings *)
let gram : nt rule = alt [ 
  (q -$$ return S);
  (x -$$ x -$$ return Q);
  (q -$$ x -$$ return Q);
  (x -$$ q -$$ return Q);
  (q -$$ q -$$ return Q);
]

let dict : string -> nt list = fun token -> [Token token]

let accept : nt -> unit option = function
  | S -> Some ()
  | _ -> None

let run tokens = Chart.parse ~dict ~gram ~accept ~tokens ()

let test s =
  let tokens = List.map (String.to_list s) ~f:(String.make 1) in
  printf "tokens: %s\n" (String.concat ~sep:" " tokens);
  printf !"res: %{sexp:int option}\n" (number_of_parses (run tokens))

let%expect_test _ =
  test "123456";
  [%expect {|
      tokens: 1 2 3 4 5 6
      res: (42)|}]
