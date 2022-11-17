open Ast

type exprval = Bool of bool | Nat of int | Str of string
type exprtype = BoolT | NatT | StrT
exception TypeError of string

let rec typecheck = function
    True -> BoolT
  | False -> BoolT
  | Not(e) -> (match typecheck e with
    | BoolT -> BoolT
    | _ -> raise (TypeError "Bool Expected"))
  | And(e1,e2) -> (match (typecheck e1,typecheck e2) with
    |  (BoolT,BoolT) -> BoolT
    | _ -> raise (TypeError "Bools Expected"))
  | Or(e1,e2) -> (match (typecheck e1,typecheck e2) with
  |  (BoolT,BoolT) -> BoolT
  | _ -> raise (TypeError "Bools Expected"))
  | If(e0,e1,e2) -> (match (typecheck e0,typecheck e1,typecheck e2) with
      |  (BoolT,BoolT,BoolT) -> BoolT
      | _ -> raise (TypeError "Bools Expected"))
  | Zero -> NatT
  | Succ(e) -> (match typecheck e with
      | NatT -> NatT
      | _ -> raise (TypeError "Nat Expected"))
  | Pred(e) -> (match typecheck e with
      | NatT -> NatT
      | _ -> raise (TypeError "Nat Expected"))
  | IsZero(e) -> (match typecheck e with
      | NatT -> BoolT
      | _ -> raise (TypeError "Nat expected"))
  | Var(_) -> StrT
  | Let(_,e1,e2) -> (match (typecheck e1,typecheck e2) with
      |  (NatT,NatT) -> NatT
      | _ -> raise (TypeError "Strings Expected"))

let string_of_type = function
  | BoolT -> "BoolT"
  | NatT -> "NatT"
  | StrT -> "StrT"

let string_of_val = function
    Bool b -> if b then "true" else "false"
  | Nat n -> string_of_int n
  | Str s -> s

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "pred(" ^ string_of_expr e ^ ")"
  | IsZero(e) -> "iszero(" ^ string_of_expr e ^ ")"
  | Var(e) -> e
  | Let(e0,e1,e2) -> "let " ^ e0 ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception NoRuleApplies
exception PredOfZero

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false
  
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e) -> e
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e) -> e
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) when is_nv e -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e -> False    
  | IsZero(e) -> let e' = trace1 e in IsZero(e')    
  | Let(e0,e1,e2) -> let e1' = trace1 e1 in Let(e0,e1',e2)
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

let rec subst e v x =  match e with
    Var(y) -> if x = y then v else e
  | True -> True
  | False -> False
  | Not(e1) -> Not(subst e1 v x)
  | And(e1,e2) -> And(subst e1 v x, subst e2 v x)
  | Or(e1,e2) -> Or(subst e1 v x, subst e2 v x)
  | If(e0,e1,e2) -> If(subst e0 v x, subst e1 v x, subst e2 v x)
  | Zero -> Zero
  | Succ(e1) -> Succ(subst e1 v x)
  | Pred(e1) -> Pred(subst e1 v x)
  | IsZero(e1) -> IsZero(subst e1 v x)
  | Let(e0,e1,e2) -> Let(e0, subst e1 v x, subst e2 v x)


let rec eval = function
    True -> Bool true
  | False -> Bool false
  | Not(e) -> (match eval e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not on nat")
    )
  | And(e1,e2) -> (match (eval e1,eval e2) with
        (Bool b1,Bool b2) -> Bool (b1 && b2)
      | _ -> raise (TypeError "Or on nat")
    )
  | Or(e1,e2) -> (match (eval e1,eval e2) with
        (Bool b1,Bool b2) -> Bool (b1 || b2)
      | _ -> raise (TypeError "Or on nat")
    ) 
  | If(e0,e1,e2) -> (match eval e0 with
        Bool b -> if b then eval e1 else eval e2
      | _ -> raise (TypeError "If on nat guard")
    )
  | Zero -> Nat 0
  | Succ(e) -> (match eval e with
        Nat n -> Nat (n+1)
      | _ -> raise (TypeError "Succ on bool")
    )
  | Pred(e) -> (match eval e with
      | Nat n when n>0 -> Nat (n-1)
      | _ -> raise (TypeError "pred on 0")
    )
  | IsZero(e) -> (match eval e with
      | Nat n -> Bool (n=0)
      | _ -> raise (TypeError "IsZero on bool")
    )
  | Var(e) -> Str e
  | Let(e0,e1,e2) -> eval (subst e2 e0 e1)
;;
