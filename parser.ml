type token =
  | LPAREN
  | RPAREN
  | SEMISEMI
  | PLUS
  | MULT
  | LT
  | ANDB
  | ORB
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | LET
  | IN
  | EQ
  | AND
  | RARROW
  | FUN
  | INTV of (int)
  | ID of (Syntax.id)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Syntax
# 29 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* SEMISEMI *);
  260 (* PLUS *);
  261 (* MULT *);
  262 (* LT *);
  263 (* ANDB *);
  264 (* ORB *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* TRUE *);
  269 (* FALSE *);
  270 (* LET *);
  271 (* IN *);
  272 (* EQ *);
  273 (* AND *);
  274 (* RARROW *);
  275 (* FUN *);
    0|]

let yytransl_block = [|
  276 (* INTV *);
  277 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\004\000\004\000\002\000\002\000\
\002\000\002\000\008\000\006\000\007\000\007\000\009\000\009\000\
\010\000\010\000\011\000\011\000\012\000\012\000\013\000\013\000\
\014\000\014\000\014\000\014\000\014\000\005\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\005\000\003\000\001\000\001\000\
\001\000\001\000\004\000\004\000\003\000\001\000\003\000\001\000\
\003\000\001\000\003\000\001\000\003\000\001\000\002\000\001\000\
\001\000\001\000\001\000\001\000\003\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\026\000\027\000\000\000\000\000\
\025\000\028\000\031\000\000\000\002\000\007\000\008\000\009\000\
\010\000\000\000\000\000\000\000\000\000\000\000\024\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\023\000\000\000\029\000\000\000\000\000\
\004\000\000\000\000\000\003\000\000\000\013\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000\011\000\000\000\
\000\000\030\000\005\000"

let yydgoto = "\002\000\
\011\000\012\000\044\000\028\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\022\000\023\000"

let yysindex = "\004\000\
\003\255\000\000\037\255\037\255\000\000\000\000\244\254\245\254\
\000\000\000\000\000\000\008\255\000\000\000\000\000\000\000\000\
\000\000\005\255\021\255\002\255\024\255\006\255\000\000\244\254\
\028\255\029\255\025\255\018\255\016\255\000\000\006\255\006\255\
\006\255\006\255\006\255\000\000\030\255\000\000\037\255\037\255\
\000\000\244\254\037\255\000\000\037\255\000\000\000\000\024\255\
\043\255\006\255\033\255\031\255\011\255\000\000\000\000\037\255\
\244\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\157\255\147\255\121\255\089\255\057\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\105\255\
\137\255\073\255\000\000\040\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\051\000\234\255\000\000\000\000\000\000\000\000\
\035\000\037\000\036\000\040\000\047\000\237\255"

let yytablesize = 174
let yytable = "\025\000\
\026\000\037\000\036\000\003\000\001\000\033\000\003\000\034\000\
\027\000\029\000\030\000\004\000\031\000\041\000\005\000\006\000\
\007\000\005\000\006\000\053\000\041\000\008\000\009\000\010\000\
\042\000\009\000\010\000\032\000\035\000\038\000\036\000\042\000\
\043\000\045\000\059\000\051\000\052\000\003\000\039\000\054\000\
\040\000\055\000\006\000\056\000\043\000\004\000\033\000\057\000\
\005\000\006\000\024\000\013\000\058\000\006\000\006\000\008\000\
\009\000\010\000\022\000\022\000\022\000\022\000\022\000\022\000\
\022\000\046\000\022\000\022\000\047\000\049\000\022\000\022\000\
\048\000\022\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\050\000\021\000\021\000\000\000\000\000\021\000\021\000\
\000\000\021\000\020\000\020\000\020\000\000\000\020\000\020\000\
\020\000\000\000\020\000\020\000\000\000\000\000\020\000\020\000\
\000\000\020\000\019\000\019\000\019\000\000\000\019\000\019\000\
\019\000\000\000\019\000\019\000\000\000\000\000\019\000\019\000\
\000\000\019\000\018\000\018\000\000\000\000\000\000\000\018\000\
\018\000\000\000\018\000\018\000\000\000\000\000\018\000\018\000\
\000\000\018\000\017\000\017\000\000\000\000\000\000\000\017\000\
\017\000\000\000\017\000\017\000\016\000\016\000\017\000\017\000\
\000\000\017\000\016\000\000\000\016\000\016\000\014\000\014\000\
\016\000\016\000\000\000\016\000\000\000\000\000\014\000\014\000\
\000\000\000\000\014\000\014\000\000\000\014\000"

let yycheck = "\003\000\
\004\000\024\000\022\000\001\001\001\000\004\001\001\001\006\001\
\021\001\021\001\003\001\009\001\008\001\003\001\012\001\013\001\
\014\001\012\001\013\001\042\000\003\001\019\001\020\001\021\001\
\014\001\020\001\021\001\007\001\005\001\002\001\050\000\014\001\
\015\001\018\001\057\000\039\000\040\000\001\001\010\001\043\000\
\016\001\045\000\003\001\011\001\015\001\009\001\004\001\017\001\
\012\001\013\001\014\001\001\000\056\000\014\001\015\001\019\001\
\020\001\021\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\031\000\010\001\011\001\032\000\034\000\014\001\015\001\
\033\000\017\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\035\000\010\001\011\001\255\255\255\255\014\001\015\001\
\255\255\017\001\002\001\003\001\004\001\255\255\006\001\007\001\
\008\001\255\255\010\001\011\001\255\255\255\255\014\001\015\001\
\255\255\017\001\002\001\003\001\004\001\255\255\006\001\007\001\
\008\001\255\255\010\001\011\001\255\255\255\255\014\001\015\001\
\255\255\017\001\002\001\003\001\255\255\255\255\255\255\007\001\
\008\001\255\255\010\001\011\001\255\255\255\255\014\001\015\001\
\255\255\017\001\002\001\003\001\255\255\255\255\255\255\007\001\
\008\001\255\255\010\001\011\001\002\001\003\001\014\001\015\001\
\255\255\017\001\008\001\255\255\010\001\011\001\002\001\003\001\
\014\001\015\001\255\255\017\001\255\255\255\255\010\001\011\001\
\255\255\255\255\014\001\015\001\255\255\017\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  SEMISEMI\000\
  PLUS\000\
  MULT\000\
  LT\000\
  ANDB\000\
  ORB\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  LET\000\
  IN\000\
  EQ\000\
  AND\000\
  RARROW\000\
  FUN\000\
  "

let yynames_block = "\
  INTV\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 19 "parser.mly"
                  ( Exp _1 )
# 190 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetDecl) in
    Obj.repr(
# 20 "parser.mly"
            ( _1 )
# 197 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'LetDecl) in
    Obj.repr(
# 23 "parser.mly"
                      ( Decl (_2, Some _3) )
# 205 "parser.ml"
               : 'LetDecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Decl) in
    Obj.repr(
# 24 "parser.mly"
                      ( Decl (_2, None) )
# 212 "parser.ml"
               : 'LetDecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Decl) in
    Obj.repr(
# 27 "parser.mly"
                        ( (_1, _3) :: _5 )
# 221 "parser.ml"
               : 'Decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 28 "parser.mly"
               ( [(_1, _3)] )
# 229 "parser.ml"
               : 'Decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'IfExpr) in
    Obj.repr(
# 31 "parser.mly"
           ( _1 )
# 236 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetExpr) in
    Obj.repr(
# 32 "parser.mly"
            ( _1 )
# 243 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ORExpr) in
    Obj.repr(
# 33 "parser.mly"
           ( _1 )
# 250 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FunExpr) in
    Obj.repr(
# 34 "parser.mly"
            ( _1 )
# 257 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 37 "parser.mly"
                       ( FunExp (_2, _4) )
# 265 "parser.ml"
               : 'FunExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Decl) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 40 "parser.mly"
                     ( LetExp (_2, _4) )
# 273 "parser.ml"
               : 'LetExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ANDExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ANDExpr) in
    Obj.repr(
# 43 "parser.mly"
                        ( BinOp (Or, _1, _3) )
# 281 "parser.ml"
               : 'ORExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ANDExpr) in
    Obj.repr(
# 44 "parser.mly"
            ( _1 )
# 288 "parser.ml"
               : 'ORExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'LTExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 47 "parser.mly"
                       ( BinOp (And, _1, _3) )
# 296 "parser.ml"
               : 'ANDExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 48 "parser.mly"
           ( _1 )
# 303 "parser.ml"
               : 'ANDExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 51 "parser.mly"
                   ( BinOp (Lt, _1, _3) )
# 311 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 52 "parser.mly"
          ( _1 )
# 318 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 55 "parser.mly"
                     ( BinOp (Plus, _1, _3) )
# 326 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 56 "parser.mly"
          ( _1 )
# 333 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 59 "parser.mly"
                       ( BinOp (Mult, _1, _3) )
# 341 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 60 "parser.mly"
            ( _1 )
# 348 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 63 "parser.mly"
                  ( AppExp (_1, _2) )
# 356 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 64 "parser.mly"
          ( _1 )
# 363 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
         ( ILit _1 )
# 370 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
         ( BLit true )
# 376 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
          ( BLit false )
# 382 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 70 "parser.mly"
       ( Var _1 )
# 389 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 71 "parser.mly"
                       ( _2 )
# 396 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 74 "parser.mly"
                                ( IfExp (_2, _4, _6) )
# 405 "parser.ml"
               : 'IfExpr))
(* Entry toplevel *)
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
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.program)
