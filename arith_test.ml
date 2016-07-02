open Core.Std

module Arith = Arith_expression_example

let split s =
  List.filter (String.split s ~on:' ') ~f:(function "" -> false | _ -> true)

let run s =
  let words = split s in
  printf "words: %s\n" (String.concat ~sep:" " words);
  let res = Arith.run words in
  printf !"res: %{sexp:Arith.res}\n" res

let%expect_test _ =
  run "a";
  [%expect {|
    words: a
    res: (Succ (Var a))|}]

let%expect_test _ =
  run "a + b";
  [%expect {|
    words: a + b
    res: (Succ (Add (Var a) (Var b)))|}]

let%expect_test _ =
  run "a + b + c";
  [%expect {|
    words: a + b + c
    res: (Amb (number_parses 2)
     ((Add (Add (Var a) (Var b)) (Var c)) (Add (Var a) (Add (Var b) (Var c)))))|}]

let%expect_test _ =
  run "( a + b ) + c";
  [%expect {|
    words: ( a + b ) + c
    res: (Succ (Add (Add (Var a) (Var b)) (Var c)))|}]

let%expect_test _ =
  run "a + ( b + c )";
  [%expect {|
    words: a + ( b + c )
    res: (Succ (Add (Var a) (Add (Var b) (Var c))))|}]

let%expect_test _ =
  run "a + b * c * ( d + e ) + ( f + g )";
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
  run "";
  [%expect {|
    words:
    res: (Fail (position 0))|}]

let%expect_test _ =
  run "(";
  [%expect {|
    words: (
    res: (Fail (position 1))|}]

let%expect_test _ =
  run "( a";
  [%expect {|
    words: ( a
    res: (Fail (position 2))|}]

let%expect_test _ =
  run "+ (";
  [%expect {|
    words: + (
    res: (Fail (position 1))|}]

let%expect_test _ =
  run "( + )";
  [%expect {|
    words: ( + )
    res: (Fail (position 2))|}]
