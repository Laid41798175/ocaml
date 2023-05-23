type token =
  | IF
  | THEN
  | ELSE
  | FN
  | RARROW
  | DOT
  | PLUS
  | MINUS
  | LP
  | RP
  | REC
  | COMMA
  | EOF
  | RAISE
  | HANDLE
  | NUM of (int)
  | ID of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Xexp.xexp
