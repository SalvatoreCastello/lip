open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                              Big-step semantics                            *)
(*                    eval_expr : state -> expr -> exprval                    *)
(******************************************************************************)

let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false
  | Var(e) -> st e
  | Const(e) -> Nat e
  | Not(e) -> (match eval_expr st e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
    )
  | And(e1,e2) -> (match (eval_expr st e1,eval_expr st e2) with
        (Bool b1,Bool b2) -> Bool (b1 && b2)
      | _ -> raise (TypeError "Or")
    )
  | Or(e1,e2) -> (match (eval_expr st e1,eval_expr st e2) with
        (Bool b1,Bool b2) -> Bool (b1 || b2)
      | _ -> raise (TypeError "Or")
    )
  | Add(n1,n2) -> (match (eval_expr st n1, eval_expr st n2) with
        (Nat n1, Nat n2) -> Nat (n1+n2)
        | _ -> raise (TypeError "Add")
    )
  | Sub(n1,n2) -> (match (eval_expr st n1, eval_expr st n2) with
        (Nat n1, Nat n2) -> Nat (n1-n2)
        | _ -> raise (TypeError "Sub")
    )
  | Mul(n1,n2) -> (match (eval_expr st n1, eval_expr st n2) with
        (Nat n1, Nat n2) -> Nat (n1*n2)
        | _ -> raise (TypeError "Mul")
    )
  | Eq(n1,n2) -> (match (eval_expr st n1, eval_expr st n2) with
        (Nat n1, Nat n2) -> Bool (n1=n2)
        | _ -> raise (TypeError "Eq")
    )
  | Leq(n1,n2) -> (match (eval_expr st n1, eval_expr st n2) with
    (Nat n1, Nat n2) -> Bool (n1<=n2)
    | _ -> raise (TypeError "Leq")
)
;;

(******************************************************************************)
(*                            Small-step semantics                            *)
(*                             trace1 conf -> conf                            *)
(******************************************************************************)

let bot = fun x -> raise (UnboundVar x)

(* Bind associa al nome "x" il valore "v" nello stato "f", se la stringa è già presente in y allora gli lo stesso valore che ha y, con il
   valore v, altrimenti associa ad una nuova stringa il valore y. *)
let bind f x v = fun y -> if y=x then v else f y


let rec trace1 = function
    St _ ->  raise NoRuleApplies (* Stato qualunque senza comandi *)
  | Cmd(c,st) -> match c with
      | Skip -> St st (* Esegue il comando di skip e restituisce lo stato stesso. *)
      (* Assegnamento, il valore v del let prende la valutazione dell'espressione "e" e lo mette nello stato attuale,
         All'interno delle parentesi viene richiamata la bind per associare all*)
      | Assign(x,e) -> let v = eval_expr st e in St (bind st x v)
      (* Seq(" ; "), prende due comandi c1 e c2, valuta (con la small step) il comando "c1" nello stato "st", una volta valutato
         - Se questo restituice uno stato "st1", viene restituito il secondo comando "c2" con lo stato precedentemente restituito
         - Se la valutazione di "c1" restituisce un nuovo comando " c1' ", allora viene restituito un nuovo comando di sequenza
           dove c1' è il primo della sequenza, c2 viene eseguito soltanto dopo aver eseguito nuovamente "c1'" *)
      | Seq(c1,c2) -> (match trace1 (Cmd(c1,st)) with
          St st1 -> Cmd(c2,st1)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
      (* If(expression,command1,command2), viene valtuata l'espressione "e" nello stato "st"
         - Se questa espressione restituisce True allora viene eseguito il comando c1 nel ramo THEN
         - Se questa espressione restituisce False allora viene eseguito il comando c2 nel ramo ELSE
         - Qualunque altra cosa la valutazione di e restituisca, allora è un eccezione. *)
      | If(e,c1,c2) -> (match eval_expr st e with
          Bool true -> Cmd(c1,st)
        | Bool false -> Cmd(c2,st)
        | _ -> raise (TypeError "If"))
      (* While(expression,command), viene valutata l'espressione "e" nello stato "st"
        - Se questa espressione restituisce True allora viene fatto es:"x+1; while(..):x+1", quindi restituita la sequenza di
        --- Comando e nuovamente il While(expression,command), sempre all'interno di uno stato
        - Se questa espressione restituisce False allora viene restituito lo stato attuale.
        - Qualunque altra cosa la valutazione di e restituisca, allora è un eccezione. *)
      | While(e,c) ->  (match eval_expr st e with
          Bool true -> Cmd(Seq(c,While(e,c)),st)
        | Bool false -> St st
        | _ -> raise (TypeError "While"))
;;

(**********************************************************************
 trace_rec : int-> conf -> conf list
 Usage: trace_rec n t performs n steps of the small-step semantics
 **********************************************************************)
 let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1 t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

(**********************************************************************
 trace : int -> cmd -> conf list
 Usage: trace n t performs n steps of the small-step semantics
 **********************************************************************)
let trace n t = trace_rec n (Cmd(t,bot))