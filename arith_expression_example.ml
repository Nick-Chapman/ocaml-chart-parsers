open Core.Std
open Chart.Rule

module Exp = struct
  type t =
    | Add of t * t
    | Mul of t * t
    | Var of string
  [@@deriving sexp_of]
end

type res = Exp.t Chart.Res.t [@@deriving sexp_of]

type nt = Term of Exp.t | Factor of Exp.t | Word of string

let dict : string -> nt list =
  fun word ->
    [Word word]

let accept : nt -> Exp.t option =
  function
  | Term x -> Some x
  | (Factor _ | Word _) -> None

type 'a rule = (nt,'a) Chart.Rule.t

let term : Exp.t rule = select (function Term e -> Some e | _ -> None)

let factor : Exp.t rule = select (function Factor e -> Some e | _ -> None)

let var : Exp.t rule =
  select (fun nt ->
    match nt with
    | Word w when String.for_all w ~f:Char.is_alpha -> Some (Exp.Var w)
    | _ -> None)
    
let key : string -> unit rule =
  fun s ->
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

let run words = Chart.parse ~dict ~gram ~accept ~words ()
