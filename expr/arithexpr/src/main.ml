open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | And(e0,e1) -> "And(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | Or(e0,e1) -> "Or(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | Not(e0) -> "Not(" ^ (string_of_boolexpr e0) ^ ")"
  | Succ(e0) -> "Succ(" ^ (string_of_boolexpr e0) ^ ")"
  | Pred(e0) -> "Pred(" ^ (string_of_boolexpr e0) ^ ")"
  | IsZero(e0) -> "IsZero(" ^ (string_of_boolexpr e0) ^ ")"
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in ast
;;


let rec eval = function
    True -> true
  | False -> false
  | If(e0,e1,e2) -> if eval e0 then eval e1 else eval e2
  | Not(e) -> not(eval(e))
  | And(e1,e2) -> eval(e1) && eval(e2)
  | Or(e1,e2) -> eval(e1) || eval(e2)
  | Zero -> 0
  | IsZero(e) -> eval(e)
  | Succ(e) -> (eval(e)) + 1
  | Pred(e) -> (eval(e)) - 1
  | IsZero(e) -> eval(e)
;;

exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e2) -> e2
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e2) -> e2
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Succ(e)) -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies

;;

let rec trace e = try
  let e' = trace1 e
  in e::(trace e')
with NoRuleApplies -> [e]
;;