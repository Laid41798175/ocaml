type token =
  | APP
  | LAMBDA
  | DOT
  | ID of (string)
  | IN
  | EQUAL
  | LET
  | LP
  | RP
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.lexp_let
