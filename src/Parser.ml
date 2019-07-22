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

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* L_PAREN *);
  265 (* R_PAREN *);
  266 (* LAMBDA *);
  267 (* DOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* STRING *);
  260 (* SYMBOL *);
  261 (* VARIABLE *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\002\000\002\000\002\000\002\000\002\000\
\002\000\004\000\004\000\004\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\003\000\004\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\010\000\011\000\012\000\007\000\006\000\013\000\
\014\000\000\000\000\000\015\000\002\000\000\000\008\000\009\000\
\000\000\000\000\001\000\003\000\004\000\000\000\005\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000"

let yysindex = "\002\000\
\012\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\255\253\254\000\000\000\000\001\000\000\000\000\000\
\252\254\251\254\000\000\000\000\000\000\012\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\246\255\000\000\000\000\000\000"

let yytablesize = 267
let yytable = "\017\000\
\019\000\018\000\001\000\020\000\021\000\022\000\000\000\000\000\
\000\000\000\000\000\000\023\000\003\000\004\000\005\000\006\000\
\007\000\008\000\009\000\010\000\000\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\000\000\011\000"

let yycheck = "\010\000\
\000\000\005\001\001\000\014\000\009\001\011\001\255\255\255\255\
\255\255\255\255\255\255\022\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\255\255\010\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\255\255\010\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  L_PAREN\000\
  R_PAREN\000\
  LAMBDA\000\
  DOT\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  STRING\000\
  SYMBOL\000\
  VARIABLE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appable) in
    Obj.repr(
# 19 "src/Parser.mly"
               ( _1 )
# 163 "src/Parser.ml"
               : Program.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Program.term) in
    Obj.repr(
# 23 "src/Parser.mly"
         ( _1 )
# 170 "src/Parser.ml"
               : 'appable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appable) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Program.term) in
    Obj.repr(
# 24 "src/Parser.mly"
                  ( Application (_1, _2) )
# 178 "src/Parser.ml"
               : 'appable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Program.term) in
    Obj.repr(
# 28 "src/Parser.mly"
                           ( _2 )
# 185 "src/Parser.ml"
               : Program.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Program.term) in
    Obj.repr(
# 29 "src/Parser.mly"
                                ( Abstraction (_2, _4) )
# 193 "src/Parser.ml"
               : Program.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 30 "src/Parser.mly"
             ( Token (Variable _1) )
# 200 "src/Parser.ml"
               : Program.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 31 "src/Parser.mly"
           ( Token (Symbol _1) )
# 207 "src/Parser.ml"
               : Program.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primitive) in
    Obj.repr(
# 32 "src/Parser.mly"
              ( _1 )
# 214 "src/Parser.ml"
               : Program.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procedure) in
    Obj.repr(
# 33 "src/Parser.mly"
              ( _1 )
# 221 "src/Parser.ml"
               : Program.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 37 "src/Parser.mly"
        ( Token (Primitive (Int _1)) )
# 228 "src/Parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 38 "src/Parser.mly"
          ( Token (Primitive (Float _1)) )
# 235 "src/Parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "src/Parser.mly"
           ( Token (Primitive (String _1)) )
# 242 "src/Parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "src/Parser.mly"
         ( Token (Procedure Plus) )
# 248 "src/Parser.ml"
               : 'procedure))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "src/Parser.mly"
          ( Token (Procedure Minus) )
# 254 "src/Parser.ml"
               : 'procedure))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Program.term)