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

open Parsing;;
let _ = parse_error;;
# 8 "parser.mly"
exception IncorrectSelection
let whichSel = function (e, 1) -> Xexp.Fst e
      | (e, 2) -> Xexp.Snd e
      | _ -> raise IncorrectSelection
# 28 "parser.ml"
let yytransl_const = [|
  257 (* IF *);
  258 (* THEN *);
  259 (* ELSE *);
  260 (* FN *);
  261 (* RARROW *);
  262 (* DOT *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* LP *);
  266 (* RP *);
  267 (* REC *);
  268 (* COMMA *);
    0 (* EOF *);
  269 (* RAISE *);
  270 (* HANDLE *);
    0|]

let yytransl_block = [|
  271 (* NUM *);
  272 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\001\000\004\000\005\000\002\000\003\000\
\003\000\006\000\005\000\002\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\004\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\000\000\000\000\000"

let yydgoto = "\002\000\
\010\000\021\000"

let yysindex = "\255\255\
\025\255\000\000\025\255\242\254\025\255\248\254\025\255\000\000\
\000\000\000\000\046\000\073\255\007\255\038\255\006\255\121\255\
\008\255\025\255\000\000\012\255\002\255\025\255\025\255\000\000\
\025\255\016\255\000\000\005\255\025\255\089\255\121\255\105\255\
\025\255\005\255\025\255\000\000\121\255\253\254"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\067\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\000\000\000\000\072\000\000\000\
\000\000\033\000\000\000\000\000\078\000\061\000"

let yygindex = "\000\000\
\000\000\002\000"

let yytablesize = 346
let yytable = "\001\000\
\007\000\013\000\011\000\018\000\012\000\005\000\014\000\015\000\
\016\000\007\000\020\000\023\000\009\000\005\000\007\000\020\000\
\008\000\007\000\020\000\028\000\033\000\026\000\027\000\030\000\
\031\000\003\000\032\000\029\000\004\000\000\000\034\000\000\000\
\013\000\005\000\037\000\006\000\038\000\007\000\003\000\008\000\
\009\000\004\000\000\000\017\000\018\000\019\000\005\000\024\000\
\006\000\025\000\007\000\020\000\008\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\000\
\000\000\000\000\012\000\000\000\000\000\000\000\000\000\005\000\
\000\000\003\000\022\000\000\000\004\000\006\000\017\000\018\000\
\000\000\005\000\000\000\006\000\000\000\007\000\020\000\008\000\
\009\000\003\000\000\000\035\000\004\000\000\000\017\000\018\000\
\000\000\005\000\000\000\006\000\000\000\007\000\020\000\008\000\
\009\000\003\000\000\000\000\000\004\000\000\000\017\000\018\000\
\000\000\005\000\036\000\006\000\000\000\007\000\020\000\008\000\
\009\000\003\000\000\000\000\000\004\000\000\000\017\000\018\000\
\000\000\005\000\000\000\006\000\000\000\007\000\020\000\008\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\007\000\007\000\007\000\007\000\000\000\007\000\007\000\
\000\000\007\000\007\000\007\000\007\000\000\000\000\000\007\000\
\007\000\008\000\008\000\008\000\008\000\000\000\008\000\008\000\
\000\000\000\000\008\000\008\000\008\000\000\000\000\000\008\000\
\008\000\013\000\013\000\013\000\013\000\000\000\013\000\013\000\
\000\000\000\000\013\000\013\000\013\000\000\000\003\000\013\000\
\013\000\004\000\000\000\017\000\018\000\000\000\005\000\000\000\
\006\000\000\000\007\000\020\000\008\000\009\000\010\000\010\000\
\010\000\000\000\010\000\000\000\012\000\012\000\010\000\010\000\
\010\000\005\000\005\000\010\000\012\000\000\000\012\000\006\000\
\006\000\005\000\000\000\005\000\000\000\000\000\000\000\006\000\
\000\000\006\000"

let yycheck = "\001\000\
\000\000\016\001\001\000\007\001\003\000\009\001\005\000\016\001\
\007\000\013\001\014\001\005\001\016\001\009\001\013\001\014\001\
\000\000\013\001\014\001\018\000\005\001\016\001\015\001\022\000\
\023\000\001\001\025\000\016\001\004\001\255\255\029\000\255\255\
\000\000\009\001\033\000\011\001\035\000\013\001\001\001\015\001\
\016\001\004\001\255\255\006\001\007\001\000\000\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
\255\255\001\001\002\001\255\255\004\001\000\000\006\001\007\001\
\255\255\009\001\255\255\011\001\255\255\013\001\014\001\015\001\
\016\001\001\001\255\255\003\001\004\001\255\255\006\001\007\001\
\255\255\009\001\255\255\011\001\255\255\013\001\014\001\015\001\
\016\001\001\001\255\255\255\255\004\001\255\255\006\001\007\001\
\255\255\009\001\010\001\011\001\255\255\013\001\014\001\015\001\
\016\001\001\001\255\255\255\255\004\001\255\255\006\001\007\001\
\255\255\009\001\255\255\011\001\255\255\013\001\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\255\255\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\255\255\255\255\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\255\255\255\255\010\001\011\001\012\001\255\255\001\001\015\001\
\016\001\004\001\255\255\006\001\007\001\255\255\009\001\255\255\
\011\001\255\255\013\001\014\001\015\001\016\001\002\001\003\001\
\004\001\255\255\006\001\255\255\002\001\003\001\010\001\011\001\
\012\001\002\001\003\001\015\001\010\001\255\255\012\001\002\001\
\003\001\010\001\255\255\012\001\255\255\255\255\255\255\010\001\
\255\255\012\001"

let yynames_const = "\
  IF\000\
  THEN\000\
  ELSE\000\
  FN\000\
  RARROW\000\
  DOT\000\
  PLUS\000\
  MINUS\000\
  LP\000\
  RP\000\
  REC\000\
  COMMA\000\
  EOF\000\
  RAISE\000\
  HANDLE\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Xexp.xexp) in
    Obj.repr(
# 32 "parser.mly"
                  (_1)
# 210 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Xexp.xexp) in
    Obj.repr(
# 35 "parser.mly"
               (_2)
# 217 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 36 "parser.mly"
        (Xexp.Num _1)
# 224 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parser.mly"
       (Xexp.Var (_1))
# 231 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Xexp.xexp) in
    Obj.repr(
# 38 "parser.mly"
                      (Xexp.Fn(_2,_4))
# 239 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Xexp.xexp) in
    Obj.repr(
# 39 "parser.mly"
                          (Xexp.Fnr(_2, _3, _5))
# 248 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Xexp.xexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Xexp.xexp) in
    Obj.repr(
# 40 "parser.mly"
                        (Xexp.App(_1,_2))
# 256 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Xexp.xexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Xexp.xexp) in
    Obj.repr(
# 41 "parser.mly"
                   (Xexp.Add(_1,_3))
# 264 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Xexp.xexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                 (whichSel (_1,_3))
# 272 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Xexp.xexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Xexp.xexp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Xexp.xexp) in
    Obj.repr(
# 43 "parser.mly"
                                (Xexp.Ifp(_2,_4,_6))
# 281 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Xexp.xexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Xexp.xexp) in
    Obj.repr(
# 44 "parser.mly"
                          (Xexp.Pair (_2, _4))
# 289 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Xexp.xexp) in
    Obj.repr(
# 45 "parser.mly"
               (Xexp.Raise _2)
# 296 "parser.ml"
               : Xexp.xexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Xexp.xexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Xexp.xexp) in
    Obj.repr(
# 46 "parser.mly"
                        (Xexp.Handle (_1, _3, _4))
# 305 "parser.ml"
               : Xexp.xexp))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Xexp.xexp)
;;
