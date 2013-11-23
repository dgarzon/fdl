type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | TAB
  | SEMI
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | MOVE
  | COPY
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | NOT
  | AND
  | OR
  | CONTINUE
  | BREAK
  | RETURN
  | IF
  | THEN
  | ELSE
  | FOR
  | IN
  | WHILE
  | DO
  | DEF
  | VOID
  | INT
  | STR
  | DICT
  | LIST
  | PATH
  | BOOL
  | TRASH
  | TRUE
  | FALSE
  | PRINT
  | LIT_INT of (int)
  | LIT_STR of (string)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 59 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRACE *);
  260 (* RBRACE *);
  261 (* LBRACK *);
  262 (* RBRACK *);
  263 (* COMMA *);
  264 (* TAB *);
  265 (* SEMI *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* TIMES *);
  269 (* DIVIDE *);
  270 (* ASSIGN *);
  271 (* MOVE *);
  272 (* COPY *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* LT *);
  276 (* LEQ *);
  277 (* GT *);
  278 (* GEQ *);
  279 (* NOT *);
  280 (* AND *);
  281 (* OR *);
  282 (* CONTINUE *);
  283 (* BREAK *);
  284 (* RETURN *);
  285 (* IF *);
  286 (* THEN *);
  287 (* ELSE *);
  288 (* FOR *);
  289 (* IN *);
  290 (* WHILE *);
  291 (* DO *);
  292 (* DEF *);
  293 (* VOID *);
  294 (* INT *);
  295 (* STR *);
  296 (* DICT *);
  297 (* LIST *);
  298 (* PATH *);
  299 (* BOOL *);
  300 (* TRASH *);
  301 (* TRUE *);
  302 (* FALSE *);
  303 (* PRINT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  304 (* LIT_INT *);
  305 (* LIT_STR *);
  306 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\008\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\006\000\006\000\010\000\
\010\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\007\000\007\000\011\000\011\000\011\000\011\000\011\000\013\000\
\013\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\014\000\014\000\015\000\015\000\016\000\
\016\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\000\000\001\000\001\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\000\000\001\000\001\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\000\000\002\000\002\000\003\000\006\000\008\000\003\000\000\000\
\001\000\001\000\001\000\003\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\001\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\003\000\005\000\006\000\009\000\
\010\000\011\000\008\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\027\000\029\000\
\031\000\032\000\030\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\016\000\019\000\
\020\000\021\000\018\000\017\000\000\000\000\000\000\000\015\000\
\024\000\033\000\000\000\000\000\025\000\004\000\000\000\000\000\
\000\000\000\000\042\000\043\000\000\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\044\000\000\000\036\000\000\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\048\000\049\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\059\000\000\000\000\000\000\000\000\000\
\000\000\038\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\021\000\044\000\058\000\060\000\045\000\
\046\000\059\000\070\000\071\000\075\000\073\000\099\000\100\000"

let yysindex = "\001\000\
\000\000\000\000\016\000\031\000\236\254\240\254\245\254\252\254\
\005\255\013\255\016\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\255\053\255\061\255\062\255\
\065\255\087\255\088\255\089\255\100\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\044\000\057\255\058\255\059\255\
\067\255\068\255\069\255\051\255\121\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\127\255\044\000\038\000\000\000\
\000\000\000\000\038\000\028\255\000\000\000\000\002\255\002\255\
\137\255\002\255\000\000\000\000\236\255\000\000\244\255\015\000\
\041\255\015\000\130\255\002\255\002\000\002\255\002\255\002\255\
\002\255\000\000\002\255\002\255\002\255\002\255\002\255\002\255\
\002\255\002\255\002\255\002\255\000\000\002\255\000\000\148\255\
\000\000\015\000\138\255\136\255\015\000\015\000\015\000\030\255\
\030\255\000\000\000\000\028\000\028\000\232\255\232\255\232\255\
\232\255\140\255\118\255\000\000\002\255\044\255\015\000\131\255\
\044\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\149\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\149\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\161\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\255\000\000\
\000\000\000\000\036\255\000\000\000\000\000\000\000\000\155\255\
\000\000\000\000\000\000\000\000\093\255\000\000\000\000\052\255\
\000\000\163\255\000\000\000\000\000\000\171\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\255\000\000\172\255\003\255\120\255\058\000\114\255\
\135\255\000\000\000\000\223\255\229\255\169\255\175\255\196\255\
\202\255\174\255\000\000\000\000\000\000\000\000\012\255\040\255\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\205\255\000\000\000\000\000\000\000\000\000\000\000\000\
\125\000\000\000\173\255\193\255\000\000\089\000\000\000\000\000"

let yytablesize = 343
let yytable = "\072\000\
\074\000\001\000\077\000\057\000\056\000\064\000\063\000\061\000\
\056\000\056\000\064\000\056\000\096\000\065\000\098\000\101\000\
\102\000\103\000\065\000\104\000\105\000\106\000\107\000\108\000\
\109\000\110\000\111\000\112\000\113\000\022\000\072\000\062\000\
\063\000\023\000\120\000\022\000\022\000\122\000\024\000\023\000\
\023\000\085\000\086\000\037\000\037\000\025\000\093\000\094\000\
\063\000\067\000\068\000\069\000\053\000\119\000\026\000\064\000\
\065\000\060\000\060\000\022\000\022\000\030\000\027\000\023\000\
\023\000\028\000\029\000\037\000\037\000\031\000\032\000\064\000\
\065\000\033\000\066\000\067\000\068\000\069\000\022\000\022\000\
\022\000\022\000\023\000\023\000\023\000\023\000\037\000\037\000\
\037\000\037\000\066\000\067\000\068\000\069\000\045\000\034\000\
\035\000\036\000\045\000\045\000\037\000\045\000\045\000\045\000\
\045\000\045\000\047\000\048\000\049\000\045\000\045\000\045\000\
\045\000\045\000\045\000\046\000\050\000\051\000\052\000\046\000\
\046\000\058\000\046\000\046\000\046\000\058\000\058\000\054\000\
\058\000\055\000\046\000\046\000\046\000\046\000\046\000\046\000\
\047\000\076\000\095\000\116\000\047\000\047\000\117\000\047\000\
\047\000\047\000\094\000\118\000\066\000\115\000\012\000\047\000\
\047\000\047\000\047\000\047\000\047\000\083\000\084\000\085\000\
\086\000\121\000\013\000\040\000\087\000\088\000\089\000\090\000\
\091\000\092\000\052\000\041\000\062\000\063\000\052\000\052\000\
\053\000\052\000\056\000\061\000\053\000\053\000\114\000\053\000\
\000\000\052\000\052\000\052\000\052\000\052\000\052\000\053\000\
\053\000\053\000\053\000\053\000\053\000\054\000\000\000\000\000\
\000\000\054\000\054\000\055\000\054\000\000\000\000\000\055\000\
\055\000\000\000\055\000\000\000\054\000\054\000\054\000\054\000\
\054\000\054\000\055\000\055\000\055\000\055\000\055\000\055\000\
\050\000\000\000\000\000\000\000\050\000\050\000\051\000\050\000\
\000\000\000\000\051\000\051\000\078\000\051\000\000\000\050\000\
\050\000\083\000\084\000\085\000\086\000\051\000\051\000\000\000\
\000\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\000\000\000\000\000\000\087\000\088\000\089\000\090\000\
\091\000\092\000\097\000\083\000\084\000\085\000\086\000\000\000\
\000\000\000\000\087\000\088\000\089\000\090\000\091\000\092\000\
\083\000\084\000\085\000\086\000\000\000\000\000\000\000\087\000\
\088\000\089\000\090\000\091\000\092\000\083\000\084\000\085\000\
\086\000\000\000\000\000\000\000\000\000\000\000\089\000\090\000\
\091\000\092\000\000\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\057\000\000\000\000\000\000\000\057\000\
\057\000\000\000\057\000\014\000\015\000\016\000\017\000\018\000\
\019\000\020\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\038\000\039\000\040\000\041\000\042\000\043\000"

let yycheck = "\063\000\
\064\000\001\000\066\000\055\000\002\001\002\001\005\001\059\000\
\006\001\007\001\007\001\009\001\076\000\002\001\078\000\079\000\
\080\000\081\000\007\001\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\092\000\050\001\094\000\004\001\
\005\001\050\001\118\000\004\001\005\001\121\000\050\001\004\001\
\005\001\012\001\013\001\004\001\005\001\050\001\006\001\007\001\
\005\001\048\001\049\001\050\001\002\001\117\000\050\001\028\001\
\029\001\006\001\007\001\028\001\029\001\009\001\050\001\028\001\
\029\001\050\001\050\001\028\001\029\001\009\001\009\001\028\001\
\029\001\009\001\047\001\048\001\049\001\050\001\047\001\048\001\
\049\001\050\001\047\001\048\001\049\001\050\001\047\001\048\001\
\049\001\050\001\047\001\048\001\049\001\050\001\002\001\009\001\
\009\001\009\001\006\001\007\001\001\001\009\001\010\001\011\001\
\012\001\013\001\050\001\050\001\050\001\017\001\018\001\019\001\
\020\001\021\001\022\001\002\001\050\001\050\001\050\001\006\001\
\007\001\002\001\009\001\010\001\011\001\006\001\007\001\007\001\
\009\001\003\001\017\001\018\001\019\001\020\001\021\001\022\001\
\002\001\001\001\009\001\002\001\006\001\007\001\007\001\009\001\
\010\001\011\001\007\001\030\001\000\000\002\001\002\001\017\001\
\018\001\019\001\020\001\021\001\022\001\010\001\011\001\012\001\
\013\001\031\001\002\001\009\001\017\001\018\001\019\001\020\001\
\021\001\022\001\002\001\009\001\002\001\002\001\006\001\007\001\
\002\001\009\001\054\000\006\001\006\001\007\001\094\000\009\001\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\017\001\
\018\001\019\001\020\001\021\001\022\001\002\001\255\255\255\255\
\255\255\006\001\007\001\002\001\009\001\255\255\255\255\006\001\
\007\001\255\255\009\001\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\017\001\018\001\019\001\020\001\021\001\022\001\
\002\001\255\255\255\255\255\255\006\001\007\001\002\001\009\001\
\255\255\255\255\006\001\007\001\001\001\009\001\255\255\017\001\
\018\001\010\001\011\001\012\001\013\001\017\001\018\001\255\255\
\255\255\014\001\015\001\016\001\009\001\010\001\011\001\012\001\
\013\001\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\010\001\011\001\012\001\013\001\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\010\001\011\001\012\001\
\013\001\255\255\255\255\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\255\255\036\001\037\001\038\001\039\001\040\001\
\041\001\042\001\043\001\002\001\255\255\255\255\255\255\006\001\
\007\001\255\255\009\001\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001\037\001\038\001\039\001\040\001\041\001\042\001\
\043\001\038\001\039\001\040\001\041\001\042\001\043\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  COMMA\000\
  TAB\000\
  SEMI\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  MOVE\000\
  COPY\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  NOT\000\
  AND\000\
  OR\000\
  CONTINUE\000\
  BREAK\000\
  RETURN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FOR\000\
  IN\000\
  WHILE\000\
  DO\000\
  DEF\000\
  VOID\000\
  INT\000\
  STR\000\
  DICT\000\
  LIST\000\
  PATH\000\
  BOOL\000\
  TRASH\000\
  TRUE\000\
  FALSE\000\
  PRINT\000\
  EOF\000\
  "

let yynames_block = "\
  LIT_INT\000\
  LIT_STR\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "parser.mly"
    ( [], [] )
# 355 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 32 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 363 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 33 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 371 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'return_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 37 "parser.mly"
       ({
        return = _2;
        fname = _3;
        formals = _5;
        fnlocals = List.rev _8;
        body = List.rev _9 })
# 387 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                ( VoidType )
# 393 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                ( IntType )
# 399 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                ( BoolType )
# 405 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                ( PathType )
# 411 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                ( StrType )
# 417 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( DictType )
# 423 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                ( ListType )
# 429 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
    ( [] )
# 435 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 55 "parser.mly"
                    ( List.rev _1 )
# 442 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 58 "parser.mly"
                               ( [_1] )
# 449 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 59 "parser.mly"
                               ( _3 :: _1 )
# 457 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                  ( { vtype = IntType;  vname = _2; } )
# 464 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                  ( { vtype = BoolType; vname = _2; } )
# 471 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                  ( { vtype = PathType; vname = _2; } )
# 478 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                  ( { vtype = StrType;  vname = _2; } )
# 485 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                  ( { vtype = DictType; vname = _2; } )
# 492 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                  ( { vtype = ListType; vname = _2; } )
# 499 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
    ( [] )
# 505 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 72 "parser.mly"
                    ( List.rev _1 )
# 512 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 75 "parser.mly"
                       ( [_1] )
# 519 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 76 "parser.mly"
                       ( _2 :: _1 )
# 527 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 81 "parser.mly"
                    ( { vtype = VoidType;  vname = _2; } )
# 534 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 82 "parser.mly"
                     ( { vtype = IntType;  vname = _2; } )
# 541 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 83 "parser.mly"
                   ( { vtype = BoolType; vname = _2; } )
# 548 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "parser.mly"
                   ( { vtype = StrType;  vname = _2; } )
# 555 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 85 "parser.mly"
                   ( { vtype = PathType; vname = _2; } )
# 562 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
                   ( { vtype = DictType; vname = _2; } )
# 569 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parser.mly"
                   ( { vtype = ListType; vname = _2; } )
# 576 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
    ( [] )
# 582 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 91 "parser.mly"
                     ( _2 :: _1 )
# 590 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                                                   ( Expr(_1) )
# 597 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 96 "parser.mly"
                                                   ( Return(_2) )
# 604 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 97 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 612 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 98 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 621 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                                                   ( Print(_2) )
# 628 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                  ( Noexpr )
# 634 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                  ( _1 )
# 641 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 107 "parser.mly"
                                   ( LitInt(_1) )
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
                                   ( LitStr(_1) )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_items) in
    Obj.repr(
# 109 "parser.mly"
                                   ( List(_2) )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
                                   ( Id(_1) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 685 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 693 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 701 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 709 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 717 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 725 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 741 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 749 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                                   ( Assign(_1, _3) )
# 757 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                                   ( Copy(_1,   _3) )
# 765 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                   ( Assign(_1, _3) )
# 773 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 124 "parser.mly"
                                   ( Call(_1,   _3) )
# 781 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                                   ( Item(_1) )
# 788 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'list_items) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_items) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Seq(_1, Comma, _3) )
# 796 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                    ( [] )
# 802 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 132 "parser.mly"
                    ( List.rev _1 )
# 809 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                              ( [_1] )
# 816 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                              ( _3 :: _1 )
# 824 "parser.ml"
               : 'actuals_list))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
