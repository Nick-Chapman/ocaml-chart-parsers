open Core.Std
open Chart.Rule

module Exp = struct
  type t = | Add of t * t | Mul of t * t | Var of string [@@deriving sexp_of]
end

type res = Exp.t Chart.Res.t [@@deriving sexp_of]

type nt = Term of Exp.t | Factor of Exp.t | Token of string

type 'a rule = (nt,'a) Chart.Rule.t

let term : Exp.t rule = select (function Term e -> Some e | _ -> None)

let factor : Exp.t rule = select (function Factor e -> Some e | _ -> None)

let var : Exp.t rule =
  select (fun nt ->
    match nt with
    | Token w when String.for_all w ~f:Char.is_alpha -> Some (Exp.Var w)
    | _ -> None)
    
let key : string -> unit rule = fun s ->
  select (fun nt ->
    match nt with
    | Token w when s=w -> Some ()
    | _ -> None)

let gram : nt rule =
  alt [
    (factor @> key "*" @> factor @> return (fun e2 () e1 -> Factor (Exp.Mul (e1,e2))));
    (term   @> key "+" @> term   @> return (fun e2 () e1 -> Term (Exp.Add (e1,e2))));
    (var    >>| fun e -> Factor e);
    (factor >>| fun e -> Term e);
    (key "(" -$$ term @> key ")" -$$ return (fun e -> Factor e));
  ]

let dict : string -> nt list = fun token -> [Token token]

let accept : nt -> Exp.t option = function
  | Term x -> Some x
  | (Factor _ | Token _) -> None
    
let run tokens = Chart.parse ~dict ~gram ~accept ~tokens ()

let split s =
  List.filter (String.split s ~on:' ') ~f:(function "" -> false | _ -> true)

let test s =
  let tokens = split s in
  printf "tokens: %s\n" (String.concat ~sep:" " tokens);
  printf !"res: %{sexp:res}\n" (run tokens)

let%expect_test _ =
  test "a";
  [%expect {|
    tokens: a
    res: (Succ (Var a))|}]

let%expect_test _ =
  test "a + b";
  [%expect {|
    tokens: a + b
    res: (Succ (Add (Var a) (Var b)))|}]

let%expect_test _ =
  test "a + b + c";
  [%expect {|
    tokens: a + b + c
    res: (Amb (number_parses 2)
     ((Add (Add (Var a) (Var b)) (Var c)) (Add (Var a) (Add (Var b) (Var c)))))|}]

let%expect_test _ =
  test "( a + b ) + c";
  [%expect {|
    tokens: ( a + b ) + c
    res: (Succ (Add (Add (Var a) (Var b)) (Var c)))|}]

let%expect_test _ =
  test "a + ( b + c )";
  [%expect {|
    tokens: a + ( b + c )
    res: (Succ (Add (Var a) (Add (Var b) (Var c))))|}]

let%expect_test _ =
  test "a + b * c * ( d + e ) + ( f + g )";
  [%expect {|
    tokens: a + b * c * ( d + e ) + ( f + g )
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
    tokens:
    res: (Fail (position 0))|}]

let%expect_test _ =
  test "(";
  [%expect {|
    tokens: (
    res: (Fail (position 1))|}]

let%expect_test _ =
  test "( a";
  [%expect {|
    tokens: ( a
    res: (Fail (position 2))|}]

let%expect_test _ =
  test "+ (";
  [%expect {|
    tokens: + (
    res: (Fail (position 1))|}]

let%expect_test _ =
  test "( + )";
  [%expect {|
    tokens: ( + )
    res: (Fail (position 2))|}]
