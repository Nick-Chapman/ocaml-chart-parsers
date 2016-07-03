open Core.Std
open Chart.Rule

module Exp = struct
  type t = | Add of t * t | Mul of t * t | Var of string [@@deriving sexp_of]
end

type res = Exp.t Chart.Res.t [@@deriving sexp_of]

type nt = Term of Exp.t | Factor of Exp.t | Word of string

type 'a rule = (nt,'a) Chart.Rule.t

let term : Exp.t rule = select (function Term e -> Some e | _ -> None)

let factor : Exp.t rule = select (function Factor e -> Some e | _ -> None)

let var : Exp.t rule =
  select (fun nt ->
    match nt with
    | Word w when String.for_all w ~f:Char.is_alpha -> Some (Exp.Var w)
    | _ -> None)
    
let key : string -> unit rule = fun s ->
  select (fun nt ->
    match nt with
    | Word w when s=w -> Some ()
    | _ -> None)

let gram : nt rule =
  alt [
    (factor @> key "*" @> factor @> return (fun e2 () e1 -> Factor (Exp.Mul (e1,e2))));
    (term   @> key "+" @> term   @> return (fun e2 () e1 -> Term (Exp.Add (e1,e2))));
    (var    >>| fun e -> Factor e);
    (factor >>| fun e -> Term e);
    (key "(" -$$ term @> key ")" -$$ return (fun e -> Factor e));
  ]

let dict : string -> nt list = fun word -> [Word word]

let accept : nt -> Exp.t option = function
  | Term x -> Some x
  | (Factor _ | Word _) -> None
    
let run words = Chart.parse ~dict ~gram ~accept ~words ()

let split s =
  List.filter (String.split s ~on:' ') ~f:(function "" -> false | _ -> true)

let test s =
  let words = split s in
  printf "words: %s\n" (String.concat ~sep:" " words);
  printf !"res: %{sexp:res}\n" (run words)

let%expect_test _ =
  test "a";
  [%expect {|
    words: a
    res: (Succ (Var a))|}]

let%expect_test _ =
  test "a + b";
  [%expect {|
    words: a + b
    res: (Succ (Add (Var a) (Var b)))|}]

let%expect_test _ =
  test "a + b + c";
  [%expect {|
    words: a + b + c
    res: (Amb (number_parses 2)
     ((Add (Add (Var a) (Var b)) (Var c)) (Add (Var a) (Add (Var b) (Var c)))))|}]

let%expect_test _ =
  test "( a + b ) + c";
  [%expect {|
    words: ( a + b ) + c
    res: (Succ (Add (Add (Var a) (Var b)) (Var c)))|}]

let%expect_test _ =
  test "a + ( b + c )";
  [%expect {|
    words: a + ( b + c )
    res: (Succ (Add (Var a) (Add (Var b) (Var c))))|}]

let%expect_test _ =
  test "a + b * c * ( d + e ) + ( f + g )";
  [%expect {|
    words: a + b * c * ( d + e ) + ( f + g )
    res: (Amb (number_parses 4)
     ((Add (Add (Var a) (Mul (Mul (Var b) (Var c)) (Add (Var d) (Var e))))
       (Add (Var f) (Var g)))
      (Add (Var a)
       (Add (Mul (Mul (Var b) (Var c)) (Add (Var d) (Var e)))
        (Add (Var f) (Var g))))
      (Add (Add (Var a) (Mul (Var b) (Mul (Var c) (Add (Var d) (Var e)))))
       (Add (Var f) (Var g)))
      (Add (Var a)
       (Add (Mul (Var b) (Mul (Var c) (Add (Var d) (Var e))))
        (Add (Var f) (Var g))))))|}]

let%expect_test _ =
  test "";
  [%expect {|
    words:
    res: (Fail (position 0))|}]

let%expect_test _ =
  test "(";
  [%expect {|
    words: (
    res: (Fail (position 1))|}]

let%expect_test _ =
  test "( a";
  [%expect {|
    words: ( a
    res: (Fail (position 2))|}]

let%expect_test _ =
  test "+ (";
  [%expect {|
    words: + (
    res: (Fail (position 1))|}]

let%expect_test _ =
  test "( + )";
  [%expect {|
    words: ( + )
    res: (Fail (position 2))|}]
