/* Loosely based on https://github.com/wenyuzhao/Lambda/blob/master/src/parser.mly *) */
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> SYMBOL
%token <string> VARIABLE
%token PLUS MINUS
%token L_PAREN R_PAREN
%token LAMBDA
%token DOT
%token EOF

%start main

%type <Program.term> main expr 

%%
main:
  appable; EOF { $1 }
;

appable:
    expr { $1 }
  | appable; expr { Application ($1, $2) }
;

expr:
    L_PAREN; expr; R_PAREN { $2 }
  | LAMBDA; VARIABLE; DOT; expr { Abstraction ($2, $4) }
  | VARIABLE { Token (Variable $1) }
  | SYMBOL { Token (Symbol $1) }
  | primitive { $1 }
  | procedure { $1 }
;

primitive:
    INT { Token (Primitive (Int $1)) }
  | FLOAT { Token (Primitive (Float $1)) }
  | STRING { Token (Primitive (String $1)) }
;

procedure:
    PLUS { Token (Procedure Plus) }
  | MINUS { Token (Procedure Minus) }
;