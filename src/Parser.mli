type token =
  | INT of (int)
  | FLOAT of (float)
  | STRING of (string)
  | SYMBOL of (string)
  | VARIABLE of (string)
  | PLUS
  | MINUS
  | L_PAREN
  | R_PAREN
  | LAMBDA
  | DOT
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Program.term
