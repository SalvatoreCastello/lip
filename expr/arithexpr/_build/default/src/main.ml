open Ast
type exprval = Bool of bool | Nat of int;;

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | And(e0,e1) -> "And(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Or(e0,e1) -> "Or(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Not(e0) -> "Not(" ^ (string_of_expr e0) ^ ")"
  | Succ(e0) -> "Succ(" ^ (string_of_expr e0) ^ ")"
  | Pred(e0) -> "Pred(" ^ (string_of_expr e0) ^ ")"
  | IsZero(e0) -> "IsZero(" ^ (string_of_expr e0) ^ ")"
  | Zero -> "0"
;;

let string_of_val = function
    Bool true -> "True"
  | Bool false -> "False"
  | Nat n -> "" ^ (string_of_int n)
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in ast
;;

(* eval : expr -> exprval, quindi nel not deve restituire una expr*)

let rec eval = function
    True -> Bool true
  | False -> Bool false
  | Not(e) -> (match eval e with
            | Bool b -> Bool(not b)
            | _ -> failwith "no")
  | If(e0,e1,e2) -> (match (eval e0) with
            | Bool true -> (eval e1)
            | Bool false -> (eval e2)
            | _ -> failwith "no")
  | And(e1,e2) -> ( match (eval e1,eval e2) with
            | (Bool b1, Bool b2) -> Bool(b1 && b2)
            | _ -> failwith "no")
  | Or(e1,e2) -> ( match (eval e1,eval e2) with
            | (Bool b1, Bool b2) -> Bool(b1 || b2)
            | _ -> failwith "no")
  | Zero -> Nat 0
  | IsZero(e) -> (match eval e with
            | Nat e -> Bool(e=0)
            | _ -> failwith "no")
  | Succ(e) -> ( match eval e with
            | Nat e -> Nat(e+1)
            | _ -> failwith "no")  
  | Pred(e) -> ( match eval e with
              Nat 0 -> failwith "no"
            | Nat e -> Nat(e-1)
            | _ -> failwith "no")            
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
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies

;;

let rec trace e = try
  let e' = trace1 e
  in e::(trace e')
with NoRuleApplies -> [e]
;;