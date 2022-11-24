open Ast

type exprval = Bool of bool | Nat of int
type exprtype = BoolT | NatT
type state = ide -> exprval
type conf = St of state | Cmd of cmd * state

exception TypeError of string
(*
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
      | _ -> raise (TypeError "Bools Expected"))*)

let string_of_type = function
  | BoolT -> "BoolT"
  | NatT -> "NatT"

let string_of_val = function
    Bool b -> if b then "true" else "false"
  | Nat n -> string_of_int n

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Var(e) -> e
  | Const(e) -> string_of_int e
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2
  | Add(e1,e2) -> string_of_expr e1 ^ "+" ^ string_of_expr e2
  | Sub(e1,e2) -> string_of_expr e1 ^ "-" ^ string_of_expr e2
  | Mul(e1,e2) -> string_of_expr e1 ^ "*" ^ string_of_expr e2
  | Eq(e1,e2) -> string_of_expr e1 ^ "=" ^ string_of_expr e2
  | Leq(e1,e2) -> string_of_expr e1 ^ "<=" ^ string_of_expr e2

let rec string_of_cmd = function
    Skip -> "skip"
  | Assign(x,e) -> x + ":=" + string_of_int e
  | Seq(c1,c2) -> string_of_cmd c1 + ";" + string_of_cmd c2
  | If(e,c1,c2) -> "if " + string_of_expr e + " then " + string_of_cmd c1 + " else " + string_of_cmd c2
  | While(e,c) -> "while " + string_of_expr e + " do " + string_of_cmd c


  (*
let rec string_of_state = function
    [] -> ""
  | (x,v)::xs -> x + "=" + string_of_val v + ";" + string_of_state xs

let rec string_of_conf = function
    St s -> string_of_state s
  | Cmd(c,s) -> string_of_cmd c + " | " + string_of_state s

let rec string_of_trace = function
    [] -> ""
  | c::cs -> string_of_conf c + " | " + string_of_trace cs*)






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
;;
