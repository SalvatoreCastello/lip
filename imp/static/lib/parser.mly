%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token MUL
%token EQ
%token LEQ
%token <string> ID
%token <string> CONST

%token SKIP
%token TAKES
%token SEQ
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token EOF

%token FUN
%token INT
%token RETURN

(* sequences are reduced eagerly *)
%left SKIP LBRACE ID CONST TRUE FALSE INT FUN LPAREN WHILE IF ELSE DO
%nonassoc CMDSEQ DECLSEQ

%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left MUL

%start <prog> prog
%type <decl> decl
%type <cmd> cmd
%type <expr> expr 

%%

prog:
  | c = cmd; EOF { Prog(EmptyDecl,c) }
  | d = decl; c = cmd; EOF { Prog(d,c) }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not e }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | f = ID; LPAREN; e=expr; RPAREN { Call(f,e) }
  | x = ID { Var(x) }
  | LPAREN; e = expr; RPAREN { e }
;

cmd:
  | SKIP; SEQ { Skip }
  | IF; e0 = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e0,c1,c2) }
  | WHILE; e = expr; DO; c = cmd; { While(e,c) }
  | x = ID; TAKES; e=expr; SEQ { Assign(x,e) }
  | c1 = cmd; c2 = cmd; { Seq(c1,c2) } %prec CMDSEQ
  | LPAREN; c = cmd; RPAREN { c }
  | e = expr; SEQ { Expr e }
  | LBRACE; d = decl; c = cmd; RBRACE { Decl(d,c) }

decl:
  | INT; x = ID; SEQ { IntVar(x) }
  | FUN; f = ID; LPAREN; x = ID; RPAREN; LBRACE; RETURN; e = expr; RBRACE { Fun(f,x,Skip,e) } (* function with return expr only *)
  | FUN; f = ID; LPAREN; x = ID; RPAREN; LBRACE; c = cmd; RETURN; e = expr; RBRACE { Fun(f,x,c,e) }
  | d1 = decl; d2 = decl { DSeq(d1,d2) } %prec DECLSEQ
