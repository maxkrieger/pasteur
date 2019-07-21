{
  open Parser
  exception UnexpectedCharacter
}
let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let float = digit* frac?
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
    ' ' { token lexbuf }
  | '\\' { LAMBDA }
  | '(' { L_PAREN }
  | ')' { R_PAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | id+ as x { VARIABLE }
  | eof { EOF }
  | _ { raise UnexpectedCharacter }