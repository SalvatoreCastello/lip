%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token EOF
%token NOT
%token AND
%token OR
%token SUCC
%token PRED
%token ISZERO
%token ZERO

%left OR
%left AND
%right NOT

%start <expr> prog
%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  | NOT; e = expr; { Not(e) }
  | e1=expr; AND; e2 = expr; { And(e1,e2) }
  | e1=expr; OR; e2 = expr; { Or(e1,e2) }
  | SUCC; e = expr; { Succ(e) }
  | PRED; e = expr; { Pred(e) }
  | ISZERO; e = expr; { IsZero(e) }
  | ZERO { Zero }
;