{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let variabile = letter chr*

let numbers = ['0'-'9']+
let costante = numbers*

(* Definire variabili e costanti *)

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR } 
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | "skip" { SKIP }
  | ":=" { ASSIGN }
  | ";" { SEQ }  
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }   
  | "while" { WHILE }
  | "do" { DO }
  | variabile { VAR (Lexing.lexeme lexbuf) }
  | costante { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
