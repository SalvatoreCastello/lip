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

<<<<<<< HEAD
=======
(* ELSE is not associative *)
%nonassoc ELSE

(* OR is left associative has the priority over ELSE *)
>>>>>>> main
%left OR

(* AND is left associative and has the priority over OR *)
%left AND
<<<<<<< HEAD
=======

(* NOT is right associative has the highest priority *)
>>>>>>> main
%right NOT

%start <boolExpr> prog
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
;