open Ast

type exprval = Bool of bool | Nat of int
type state = ide -> exprval
type conf = St of state | Cmd of cmd * state
type exprtype = BoolT | NatT

exception TypeError of string
exception UnboundVar of string
exception NoRuleApplies
