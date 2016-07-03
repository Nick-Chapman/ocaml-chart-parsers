open Core.Std
open Chart.Rule

(* parts-of-speech and syntactic-categories *)    
type pos = Pronoun | Det | Verb | Noun | Prep		    [@@deriving sexp_of]
type cat = Sentence | NP | VP | PP			    [@@deriving sexp_of]

(* type of parse trees *)
type tree = Pos of pos * string | Cat of cat * tree list    [@@deriving sexp_of]
type result = tree Chart.Res.t				    [@@deriving sexp_of]

(* parsers for POS and CAT *)
let pos p = select (function Pos (p',_) as x when p=p' -> Some x | _ -> None)
let cat c = select (function Cat (c',_) as x when c=c' -> Some x | _ -> None)
  
let pronoun = pos Pronoun
let det     = pos Det
let verb    = pos Verb
let noun    = pos Noun
let prep    = pos Prep

let np	    = cat NP
let vp	    = cat VP
let pp	    = cat PP

(* grammar in BNF style *)  
let bnf : (cat * (tree,tree) Chart.Rule.t list) list = [
  NP        , [pronoun];
  NP        , [det;noun];
  VP        , [verb;np];
  VP        , [verb;np;pp];
  PP        , [prep;np];
  NP        , [np;pp];
  Sentence  , [np;vp];
]

(* BNF converted to a parser rule *)
let gram =
  alt (List.map bnf ~f:(fun (lhs,rhs) ->
    sequence rhs @> return (fun xs -> Cat (lhs,xs))))

(* POS tagging for a small set of words *)  
let tags : string -> pos list = function
  |"I"                          -> [Pronoun]
  |"the"|"a"                    -> [Det]
  |"saw"                        -> [Verb]
  |"man"|"telescope"|"hill"     -> [Noun]
  |"on"|"with"                  -> [Prep]
  | _                           -> []
      
let dict : string -> tree list = fun word ->
  List.map (tags word) ~f:(fun pos -> Pos (pos,word))

let accept : tree -> tree option = function
  | Cat (Sentence,_) as x -> Some x
  | _ -> None

let run words =
  Chart.parse ~dict ~gram ~accept ~words ()

let split s =
  List.filter (String.split s ~on:' ') ~f:(function "" -> false | _ -> true)

let test s =
  let words = split s in
  printf "words: %s\n" (String.concat ~sep:" " words);
  printf !"res: %{sexp:result}\n" (run words)
    
let%expect_test _ =
  test "I saw the man with a telescope";
  [%expect {|
words: I saw the man with a telescope
res: (Amb (number_parses 2)
 ((Cat Sentence
   ((Cat NP ((Pos Pronoun I)))
    (Cat VP
     ((Pos Verb saw) (Cat NP ((Pos Det the) (Pos Noun man)))
      (Cat PP ((Pos Prep with) (Cat NP ((Pos Det a) (Pos Noun telescope)))))))))
  (Cat Sentence
   ((Cat NP ((Pos Pronoun I)))
    (Cat VP
     ((Pos Verb saw)
      (Cat NP
       ((Cat NP ((Pos Det the) (Pos Noun man)))
        (Cat PP
         ((Pos Prep with) (Cat NP ((Pos Det a) (Pos Noun telescope))))))))))))) |}]
