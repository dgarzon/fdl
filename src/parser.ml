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
  | IN
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
  | PATHNAME
  | PATHCREATED
  | PATHKIND
  | LIT_INT of (int)
  | LIT_STR of (string)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 62 "parser.ml"
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
  270 (* IN *);
  271 (* ASSIGN *);
  272 (* MOVE *);
  273 (* COPY *);
  274 (* EQ *);
  275 (* NEQ *);
  276 (* LT *);
  277 (* LEQ *);
  278 (* GT *);
  279 (* GEQ *);
  280 (* NOT *);
  281 (* AND *);
  282 (* OR *);
  283 (* CONTINUE *);
  284 (* BREAK *);
  285 (* RETURN *);
  286 (* IF *);
  287 (* THEN *);
  288 (* ELSE *);
  289 (* FOR *);
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
  304 (* PATHNAME *);
  305 (* PATHCREATED *);
  306 (* PATHKIND *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  307 (* LIT_INT *);
  308 (* LIT_STR *);
  309 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\008\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\006\000\006\000\010\000\
\010\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\007\000\007\000\011\000\011\000\011\000\011\000\011\000\013\000\
\013\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\016\000\016\000\016\000\
\014\000\014\000\015\000\015\000\017\000\017\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\000\000\001\000\001\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\000\000\001\000\001\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\000\000\002\000\002\000\003\000\006\000\008\000\003\000\000\000\
\001\000\001\000\001\000\003\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\002\000\001\000\001\000\001\000\
\001\000\003\000\000\000\001\000\001\000\003\000\002\000"

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
\000\000\062\000\063\000\064\000\061\000\035\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\000\000\036\000\000\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\048\000\049\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\060\000\000\000\000\000\000\000\000\000\000\000\038\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\021\000\044\000\058\000\060\000\045\000\
\046\000\059\000\070\000\071\000\075\000\073\000\104\000\085\000\
\105\000"

let yysindex = "\001\000\
\000\000\000\000\224\254\045\000\215\254\217\254\001\255\006\255\
\008\255\020\255\031\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\255\012\255\084\255\085\255\
\093\255\100\255\107\255\108\255\117\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\058\000\074\255\075\255\086\255\
\087\255\095\255\096\255\124\255\143\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\171\255\058\000\052\000\000\000\
\000\000\000\000\052\000\015\255\000\000\000\000\053\255\053\255\
\161\255\053\255\000\000\000\000\040\255\000\000\007\000\036\000\
\073\255\036\000\166\255\053\255\022\000\053\255\053\255\053\255\
\053\255\000\000\000\000\000\000\000\000\000\000\053\255\053\255\
\053\255\053\255\053\255\053\255\053\255\053\255\053\255\053\255\
\053\255\000\000\053\255\000\000\159\255\000\000\036\000\174\255\
\177\255\036\000\036\000\036\000\039\255\039\255\000\000\000\000\
\092\000\050\000\050\000\055\000\055\000\055\000\055\000\178\255\
\155\255\000\000\053\255\048\255\036\000\160\255\048\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\191\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\195\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\196\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\255\000\000\
\000\000\000\000\034\255\000\000\000\000\000\000\000\000\203\255\
\000\000\000\000\000\000\000\000\101\255\000\000\000\000\076\255\
\000\000\204\255\000\000\000\000\000\000\212\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\255\000\000\
\217\255\129\255\151\255\072\000\123\255\145\255\000\000\000\000\
\181\255\243\255\005\000\187\255\209\255\215\255\237\255\214\255\
\000\000\000\000\000\000\000\000\035\255\045\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\017\000\000\000\000\000\000\000\000\000\000\000\000\000\
\169\000\000\000\175\255\193\255\000\000\126\000\000\000\000\000\
\000\000"

let yytablesize = 361
let yytable = "\072\000\
\074\000\001\000\077\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\022\000\101\000\023\000\103\000\106\000\
\107\000\108\000\062\000\063\000\030\000\022\000\022\000\109\000\
\110\000\111\000\112\000\113\000\114\000\115\000\116\000\117\000\
\118\000\119\000\069\000\072\000\070\000\023\000\023\000\069\000\
\078\000\070\000\126\000\064\000\065\000\128\000\022\000\022\000\
\037\000\037\000\089\000\090\000\063\000\024\000\079\000\080\000\
\081\000\063\000\025\000\125\000\026\000\066\000\023\000\023\000\
\022\000\067\000\068\000\069\000\022\000\022\000\022\000\057\000\
\027\000\037\000\037\000\061\000\064\000\065\000\098\000\099\000\
\023\000\065\000\065\000\028\000\023\000\023\000\023\000\082\000\
\083\000\084\000\029\000\037\000\031\000\032\000\066\000\037\000\
\037\000\037\000\067\000\068\000\069\000\033\000\045\000\067\000\
\068\000\069\000\045\000\045\000\034\000\045\000\045\000\045\000\
\045\000\045\000\045\000\035\000\036\000\037\000\045\000\045\000\
\045\000\045\000\045\000\045\000\046\000\053\000\047\000\048\000\
\046\000\046\000\057\000\046\000\046\000\046\000\057\000\057\000\
\046\000\057\000\049\000\050\000\046\000\046\000\046\000\046\000\
\046\000\046\000\047\000\051\000\052\000\054\000\047\000\047\000\
\059\000\047\000\047\000\047\000\059\000\059\000\047\000\059\000\
\121\000\076\000\047\000\047\000\047\000\047\000\047\000\047\000\
\087\000\088\000\089\000\090\000\091\000\055\000\100\000\122\000\
\092\000\093\000\094\000\095\000\096\000\097\000\050\000\123\000\
\099\000\124\000\050\000\050\000\053\000\050\000\071\000\127\000\
\053\000\053\000\050\000\053\000\012\000\013\000\050\000\050\000\
\050\000\050\000\050\000\050\000\053\000\053\000\053\000\053\000\
\053\000\053\000\054\000\040\000\041\000\067\000\054\000\054\000\
\055\000\054\000\068\000\066\000\055\000\055\000\056\000\055\000\
\120\000\000\000\054\000\054\000\054\000\054\000\054\000\054\000\
\055\000\055\000\055\000\055\000\055\000\055\000\056\000\000\000\
\000\000\000\000\056\000\056\000\051\000\056\000\000\000\000\000\
\051\000\051\000\000\000\051\000\000\000\000\000\056\000\056\000\
\056\000\056\000\056\000\056\000\051\000\051\000\052\000\000\000\
\000\000\000\000\052\000\052\000\000\000\052\000\000\000\086\000\
\087\000\088\000\089\000\090\000\091\000\000\000\052\000\052\000\
\092\000\093\000\094\000\095\000\096\000\097\000\102\000\087\000\
\088\000\089\000\090\000\091\000\000\000\000\000\000\000\092\000\
\093\000\094\000\095\000\096\000\097\000\087\000\088\000\089\000\
\090\000\091\000\000\000\000\000\000\000\092\000\093\000\094\000\
\095\000\096\000\097\000\087\000\088\000\089\000\090\000\091\000\
\087\000\088\000\089\000\090\000\091\000\094\000\095\000\096\000\
\097\000\058\000\000\000\000\000\000\000\058\000\058\000\000\000\
\058\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\005\000\006\000\007\000\008\000\009\000\010\000\011\000\038\000\
\039\000\040\000\041\000\042\000\043\000\087\000\088\000\089\000\
\090\000"

let yycheck = "\063\000\
\064\000\001\000\066\000\036\001\037\001\038\001\039\001\040\001\
\041\001\042\001\043\001\053\001\076\000\053\001\078\000\079\000\
\080\000\081\000\004\001\005\001\009\001\004\001\005\001\087\000\
\088\000\089\000\090\000\091\000\092\000\093\000\094\000\095\000\
\096\000\097\000\002\001\099\000\002\001\004\001\005\001\007\001\
\001\001\007\001\124\000\029\001\030\001\127\000\029\001\030\001\
\004\001\005\001\012\001\013\001\005\001\053\001\015\001\016\001\
\017\001\005\001\053\001\123\000\053\001\047\001\029\001\030\001\
\047\001\051\001\052\001\053\001\051\001\052\001\053\001\055\000\
\053\001\029\001\030\001\059\000\029\001\030\001\006\001\007\001\
\047\001\006\001\007\001\053\001\051\001\052\001\053\001\048\001\
\049\001\050\001\053\001\047\001\009\001\009\001\047\001\051\001\
\052\001\053\001\051\001\052\001\053\001\009\001\002\001\051\001\
\052\001\053\001\006\001\007\001\009\001\009\001\010\001\011\001\
\012\001\013\001\014\001\009\001\009\001\001\001\018\001\019\001\
\020\001\021\001\022\001\023\001\002\001\002\001\053\001\053\001\
\006\001\007\001\002\001\009\001\010\001\011\001\006\001\007\001\
\014\001\009\001\053\001\053\001\018\001\019\001\020\001\021\001\
\022\001\023\001\002\001\053\001\053\001\007\001\006\001\007\001\
\002\001\009\001\010\001\011\001\006\001\007\001\014\001\009\001\
\002\001\001\001\018\001\019\001\020\001\021\001\022\001\023\001\
\010\001\011\001\012\001\013\001\014\001\003\001\009\001\002\001\
\018\001\019\001\020\001\021\001\022\001\023\001\002\001\007\001\
\007\001\031\001\006\001\007\001\002\001\009\001\000\000\032\001\
\006\001\007\001\014\001\009\001\002\001\002\001\018\001\019\001\
\020\001\021\001\022\001\023\001\018\001\019\001\020\001\021\001\
\022\001\023\001\002\001\009\001\009\001\002\001\006\001\007\001\
\002\001\009\001\002\001\006\001\006\001\007\001\054\000\009\001\
\099\000\255\255\018\001\019\001\020\001\021\001\022\001\023\001\
\018\001\019\001\020\001\021\001\022\001\023\001\002\001\255\255\
\255\255\255\255\006\001\007\001\002\001\009\001\255\255\255\255\
\006\001\007\001\255\255\009\001\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\023\001\018\001\019\001\002\001\255\255\
\255\255\255\255\006\001\007\001\255\255\009\001\255\255\009\001\
\010\001\011\001\012\001\013\001\014\001\255\255\018\001\019\001\
\018\001\019\001\020\001\021\001\022\001\023\001\009\001\010\001\
\011\001\012\001\013\001\014\001\255\255\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\010\001\011\001\012\001\
\013\001\014\001\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\010\001\011\001\012\001\013\001\014\001\
\010\001\011\001\012\001\013\001\014\001\020\001\021\001\022\001\
\023\001\002\001\255\255\255\255\255\255\006\001\007\001\255\255\
\009\001\037\001\038\001\039\001\040\001\041\001\042\001\043\001\
\037\001\038\001\039\001\040\001\041\001\042\001\043\001\038\001\
\039\001\040\001\041\001\042\001\043\001\010\001\011\001\012\001\
\013\001"

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
  IN\000\
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
  PATHNAME\000\
  PATHCREATED\000\
  PATHKIND\000\
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
# 33 "parser.mly"
    ( [], [] )
# 372 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 34 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 380 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 35 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 388 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'return_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 39 "parser.mly"
       ({
        return = _2;
        fname = _3;
        formals = _5;
        fnlocals = List.rev _8;
        body = List.rev _9 })
# 404 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                ( VoidType )
# 410 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                ( IntType )
# 416 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                ( BoolType )
# 422 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( PathType )
# 428 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                ( StrType )
# 434 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                ( DictType )
# 440 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                ( ListType )
# 446 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
    ( [] )
# 452 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 57 "parser.mly"
                    ( List.rev _1 )
# 459 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 60 "parser.mly"
                               ( [_1] )
# 466 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 61 "parser.mly"
                               ( _3 :: _1 )
# 474 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                  ( { vtype = IntType;  vname = _2; } )
# 481 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                  ( { vtype = BoolType; vname = _2; } )
# 488 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                  ( { vtype = PathType; vname = _2; } )
# 495 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                  ( { vtype = StrType;  vname = _2; } )
# 502 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                  ( { vtype = DictType; vname = _2; } )
# 509 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                  ( { vtype = ListType; vname = _2; } )
# 516 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
    ( [] )
# 522 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 74 "parser.mly"
                    ( List.rev _1 )
# 529 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 77 "parser.mly"
                       ( [_1] )
# 536 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 78 "parser.mly"
                       ( _2 :: _1 )
# 544 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 83 "parser.mly"
                    ( { vtype = VoidType;  vname = _2; } )
# 551 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "parser.mly"
                     ( { vtype = IntType;  vname = _2; } )
# 558 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 85 "parser.mly"
                   ( { vtype = BoolType; vname = _2; } )
# 565 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
                   ( { vtype = StrType;  vname = _2; } )
# 572 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parser.mly"
                   ( { vtype = PathType; vname = _2; } )
# 579 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 88 "parser.mly"
                   ( { vtype = DictType; vname = _2; } )
# 586 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 89 "parser.mly"
                   ( { vtype = ListType; vname = _2; } )
# 593 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
    ( [] )
# 599 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 93 "parser.mly"
                     ( _2 :: _1 )
# 607 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                                                   ( Expr(_1) )
# 614 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 98 "parser.mly"
                                                   ( Return(_2) )
# 621 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 629 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 100 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 638 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                                                   ( Print(_2) )
# 645 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                  ( Noexpr )
# 651 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                  ( _1 )
# 658 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 109 "parser.mly"
                                   ( LitInt(_1) )
# 665 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
                                   ( LitStr(_1) )
# 672 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_items) in
    Obj.repr(
# 111 "parser.mly"
                                   ( List(_2) )
# 679 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "parser.mly"
                                   ( Id(_1) )
# 686 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 694 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 702 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 710 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                                   ( Binop(_1, In,       _3) )
# 726 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 734 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 742 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 750 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 766 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 774 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                   ( Assign(_1, _3) )
# 782 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                   ( Copy(_1,   _3) )
# 790 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                   ( Assign(_1, _3) )
# 798 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 127 "parser.mly"
                                   ( Call(_1,   _3) )
# 806 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathattributes) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Pathattr(_1, _2) )
# 814 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                                   ( Pathname )
# 820 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                                   ( Pathcreated )
# 826 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                                   ( Pathkind )
# 832 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                                   ( Item(_1) )
# 839 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'list_items) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_items) in
    Obj.repr(
# 137 "parser.mly"
                                   ( Seq(_1, Comma, _3) )
# 847 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                    ( [] )
# 853 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 141 "parser.mly"
                    ( List.rev _1 )
# 860 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                              ( [_1] )
# 867 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                              ( _3 :: _1 )
# 875 "parser.ml"
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
