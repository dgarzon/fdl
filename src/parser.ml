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
  | LIT_BOOL of (bool)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 63 "parser.ml"
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
  309 (* LIT_BOOL *);
  310 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\008\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\006\000\006\000\010\000\
\010\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\007\000\007\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\016\000\016\000\016\000\014\000\014\000\015\000\015\000\017\000\
\017\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\000\000\001\000\001\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\000\000\001\000\001\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\000\000\002\000\002\000\003\000\006\000\008\000\003\000\005\000\
\007\000\000\000\001\000\001\000\001\000\001\000\003\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\004\000\002\000\
\001\000\001\000\001\000\001\000\003\000\000\000\001\000\001\000\
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
\000\000\000\000\000\000\000\000\044\000\045\000\046\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\065\000\066\000\
\067\000\064\000\035\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\047\000\000\000\
\036\000\000\000\000\000\000\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\051\000\052\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\063\000\000\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\041\000\038\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\021\000\044\000\058\000\060\000\045\000\
\046\000\059\000\073\000\074\000\078\000\076\000\111\000\090\000\
\112\000"

let yysindex = "\003\000\
\000\000\000\000\128\000\133\255\217\254\228\254\245\254\014\255\
\016\255\028\255\029\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\042\255\039\255\066\255\088\255\
\094\255\095\255\096\255\101\255\112\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\141\000\060\255\063\255\068\255\
\071\255\074\255\076\255\136\255\125\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\137\255\141\000\135\000\000\000\
\000\000\000\000\135\000\020\255\000\000\000\000\004\255\004\255\
\138\255\148\255\149\255\004\255\000\000\000\000\000\000\130\255\
\000\000\073\000\102\000\021\255\102\000\142\255\004\255\004\255\
\004\255\088\000\004\255\004\255\004\255\004\255\000\000\000\000\
\000\000\000\000\000\000\004\255\004\255\004\255\004\255\004\255\
\004\255\004\255\004\255\004\255\004\255\004\255\000\000\004\255\
\000\000\211\255\116\000\225\255\000\000\102\000\152\255\154\255\
\102\000\102\000\102\000\250\254\250\254\000\000\000\000\080\255\
\130\000\130\000\051\255\051\255\051\255\051\255\155\255\132\255\
\004\255\090\255\000\000\004\255\090\255\000\255\000\000\102\000\
\159\255\090\255\090\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\192\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\198\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\199\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\047\255\000\000\
\000\000\000\000\055\255\000\000\000\000\000\000\000\000\205\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\146\255\
\000\000\000\000\072\255\000\000\217\255\000\000\000\000\000\000\
\000\000\000\000\200\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\255\000\000\226\255\
\038\255\181\255\203\255\175\255\197\255\000\000\000\000\247\255\
\053\000\071\000\253\255\019\000\025\000\047\000\234\255\000\000\
\000\000\000\000\000\000\000\000\000\000\140\000\000\000\012\255\
\082\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\243\255\000\000\000\000\000\000\000\000\000\000\000\000\
\187\000\000\000\244\255\193\255\000\000\138\000\000\000\000\000\
\000\000"

let yytablesize = 440
let yytable = "\075\000\
\077\000\138\000\072\000\001\000\082\000\094\000\095\000\072\000\
\063\000\092\000\093\000\094\000\095\000\073\000\022\000\106\000\
\107\000\108\000\073\000\110\000\113\000\114\000\115\000\062\000\
\063\000\023\000\103\000\104\000\116\000\117\000\118\000\119\000\
\120\000\121\000\122\000\123\000\124\000\125\000\126\000\060\000\
\075\000\057\000\024\000\060\000\060\000\061\000\060\000\030\000\
\064\000\065\000\022\000\022\000\066\000\067\000\069\000\070\000\
\071\000\072\000\023\000\023\000\092\000\093\000\094\000\095\000\
\096\000\134\000\068\000\025\000\136\000\026\000\069\000\070\000\
\071\000\072\000\031\000\022\000\022\000\068\000\068\000\022\000\
\022\000\027\000\028\000\023\000\023\000\037\000\037\000\023\000\
\023\000\092\000\093\000\094\000\095\000\022\000\063\000\029\000\
\032\000\022\000\022\000\022\000\022\000\023\000\033\000\034\000\
\035\000\023\000\023\000\023\000\023\000\036\000\037\000\037\000\
\037\000\047\000\037\000\037\000\048\000\135\000\064\000\065\000\
\137\000\049\000\066\000\067\000\050\000\140\000\141\000\051\000\
\037\000\052\000\083\000\054\000\037\000\037\000\037\000\037\000\
\068\000\053\000\079\000\055\000\069\000\070\000\071\000\072\000\
\084\000\085\000\086\000\048\000\080\000\081\000\105\000\048\000\
\048\000\131\000\048\000\048\000\048\000\048\000\048\000\048\000\
\132\000\104\000\133\000\048\000\048\000\048\000\048\000\048\000\
\048\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\049\000\087\000\088\000\089\000\049\000\049\000\062\000\049\000\
\049\000\049\000\062\000\062\000\049\000\062\000\139\000\074\000\
\049\000\049\000\049\000\049\000\049\000\049\000\050\000\012\000\
\013\000\070\000\050\000\050\000\061\000\050\000\050\000\050\000\
\061\000\061\000\050\000\061\000\128\000\042\000\050\000\050\000\
\050\000\050\000\050\000\050\000\092\000\093\000\094\000\095\000\
\096\000\043\000\130\000\071\000\097\000\098\000\099\000\100\000\
\101\000\102\000\092\000\093\000\094\000\095\000\096\000\069\000\
\056\000\127\000\097\000\098\000\099\000\100\000\101\000\102\000\
\053\000\000\000\000\000\000\000\053\000\053\000\056\000\053\000\
\000\000\000\000\056\000\056\000\053\000\056\000\000\000\000\000\
\053\000\053\000\053\000\053\000\053\000\053\000\056\000\056\000\
\056\000\056\000\056\000\056\000\057\000\000\000\000\000\000\000\
\057\000\057\000\058\000\057\000\000\000\000\000\058\000\058\000\
\000\000\058\000\000\000\000\000\057\000\057\000\057\000\057\000\
\057\000\057\000\058\000\058\000\058\000\058\000\058\000\058\000\
\059\000\000\000\000\000\000\000\059\000\059\000\054\000\059\000\
\000\000\000\000\054\000\054\000\000\000\054\000\000\000\000\000\
\059\000\059\000\059\000\059\000\059\000\059\000\054\000\054\000\
\055\000\000\000\000\000\000\000\055\000\055\000\000\000\055\000\
\000\000\091\000\092\000\093\000\094\000\095\000\096\000\000\000\
\055\000\055\000\097\000\098\000\099\000\100\000\101\000\102\000\
\109\000\092\000\093\000\094\000\095\000\096\000\000\000\000\000\
\000\000\097\000\098\000\099\000\100\000\101\000\102\000\092\000\
\093\000\094\000\095\000\096\000\000\000\000\000\000\000\097\000\
\098\000\099\000\100\000\101\000\102\000\092\000\093\000\094\000\
\095\000\129\000\000\000\000\000\000\000\097\000\098\000\099\000\
\100\000\101\000\102\000\092\000\093\000\094\000\095\000\096\000\
\000\000\000\000\000\000\000\000\000\000\099\000\100\000\101\000\
\102\000\053\000\000\000\000\000\000\000\053\000\053\000\053\000\
\053\000\053\000\053\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\038\000\039\000\040\000\041\000\042\000\043\000"

let yycheck = "\063\000\
\064\000\002\001\002\001\001\000\068\000\012\001\013\001\007\001\
\005\001\010\001\011\001\012\001\013\001\002\001\054\001\079\000\
\080\000\081\000\007\001\083\000\084\000\085\000\086\000\004\001\
\005\001\054\001\006\001\007\001\092\000\093\000\094\000\095\000\
\096\000\097\000\098\000\099\000\100\000\101\000\102\000\002\001\
\104\000\055\000\054\001\006\001\007\001\059\000\009\001\009\001\
\029\001\030\001\004\001\005\001\033\001\034\001\051\001\052\001\
\053\001\054\001\004\001\005\001\010\001\011\001\012\001\013\001\
\014\001\129\000\047\001\054\001\132\000\054\001\051\001\052\001\
\053\001\054\001\009\001\029\001\030\001\006\001\007\001\033\001\
\034\001\054\001\054\001\029\001\030\001\004\001\005\001\033\001\
\034\001\010\001\011\001\012\001\013\001\047\001\005\001\054\001\
\009\001\051\001\052\001\053\001\054\001\047\001\009\001\009\001\
\009\001\051\001\052\001\053\001\054\001\009\001\029\001\030\001\
\001\001\054\001\033\001\034\001\054\001\130\000\029\001\030\001\
\133\000\054\001\033\001\034\001\054\001\138\000\139\000\054\001\
\047\001\054\001\001\001\007\001\051\001\052\001\053\001\054\001\
\047\001\002\001\001\001\003\001\051\001\052\001\053\001\054\001\
\015\001\016\001\017\001\002\001\001\001\001\001\009\001\006\001\
\007\001\002\001\009\001\010\001\011\001\012\001\013\001\014\001\
\007\001\007\001\031\001\018\001\019\001\020\001\021\001\022\001\
\023\001\037\001\038\001\039\001\040\001\041\001\042\001\043\001\
\002\001\048\001\049\001\050\001\006\001\007\001\002\001\009\001\
\010\001\011\001\006\001\007\001\014\001\009\001\032\001\000\000\
\018\001\019\001\020\001\021\001\022\001\023\001\002\001\002\001\
\002\001\002\001\006\001\007\001\002\001\009\001\010\001\011\001\
\006\001\007\001\014\001\009\001\002\001\009\001\018\001\019\001\
\020\001\021\001\022\001\023\001\010\001\011\001\012\001\013\001\
\014\001\009\001\002\001\002\001\018\001\019\001\020\001\021\001\
\022\001\023\001\010\001\011\001\012\001\013\001\014\001\006\001\
\054\000\104\000\018\001\019\001\020\001\021\001\022\001\023\001\
\002\001\255\255\255\255\255\255\006\001\007\001\002\001\009\001\
\255\255\255\255\006\001\007\001\014\001\009\001\255\255\255\255\
\018\001\019\001\020\001\021\001\022\001\023\001\018\001\019\001\
\020\001\021\001\022\001\023\001\002\001\255\255\255\255\255\255\
\006\001\007\001\002\001\009\001\255\255\255\255\006\001\007\001\
\255\255\009\001\255\255\255\255\018\001\019\001\020\001\021\001\
\022\001\023\001\018\001\019\001\020\001\021\001\022\001\023\001\
\002\001\255\255\255\255\255\255\006\001\007\001\002\001\009\001\
\255\255\255\255\006\001\007\001\255\255\009\001\255\255\255\255\
\018\001\019\001\020\001\021\001\022\001\023\001\018\001\019\001\
\002\001\255\255\255\255\255\255\006\001\007\001\255\255\009\001\
\255\255\009\001\010\001\011\001\012\001\013\001\014\001\255\255\
\018\001\019\001\018\001\019\001\020\001\021\001\022\001\023\001\
\009\001\010\001\011\001\012\001\013\001\014\001\255\255\255\255\
\255\255\018\001\019\001\020\001\021\001\022\001\023\001\010\001\
\011\001\012\001\013\001\014\001\255\255\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\010\001\011\001\012\001\
\013\001\014\001\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\010\001\011\001\012\001\013\001\014\001\
\255\255\255\255\255\255\255\255\255\255\020\001\021\001\022\001\
\023\001\014\001\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\036\001\037\001\038\001\039\001\040\001\
\041\001\042\001\043\001\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001\038\001\039\001\040\001\041\001\042\001\043\001"

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
  LIT_BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
    ( [], [] )
# 401 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 35 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 409 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 36 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 417 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'return_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 40 "parser.mly"
       ({
        return = _2;
        fname = _3;
        formals = _5;
        fnlocals = List.rev _8;
        body = List.rev _9 })
# 433 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                ( VoidType )
# 439 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                ( IntType )
# 445 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( BoolType )
# 451 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                ( PathType )
# 457 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                ( StrType )
# 463 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                ( DictType )
# 469 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                ( ListType )
# 475 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
    ( [] )
# 481 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 58 "parser.mly"
                    ( List.rev _1 )
# 488 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 61 "parser.mly"
                               ( [_1] )
# 495 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 62 "parser.mly"
                               ( _3 :: _1 )
# 503 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                  ( { vtype = IntType;  vname = _2; } )
# 510 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                  ( { vtype = BoolType; vname = _2; } )
# 517 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                  ( { vtype = PathType; vname = _2; } )
# 524 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                  ( { vtype = StrType;  vname = _2; } )
# 531 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                  ( { vtype = DictType; vname = _2; } )
# 538 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
                  ( { vtype = ListType; vname = _2; } )
# 545 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
    ( [] )
# 551 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 75 "parser.mly"
                    ( List.rev _1 )
# 558 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 78 "parser.mly"
                       ( [_1] )
# 565 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 79 "parser.mly"
                       ( _2 :: _1 )
# 573 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "parser.mly"
                    ( { vtype = VoidType;  vname = _2; } )
# 580 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 85 "parser.mly"
                     ( { vtype = IntType;  vname = _2; } )
# 587 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
                   ( { vtype = BoolType; vname = _2; } )
# 594 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parser.mly"
                   ( { vtype = StrType;  vname = _2; } )
# 601 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 88 "parser.mly"
                   ( { vtype = PathType; vname = _2; } )
# 608 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 89 "parser.mly"
                   ( { vtype = DictType; vname = _2; } )
# 615 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 90 "parser.mly"
                   ( { vtype = ListType; vname = _2; } )
# 622 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
    ( [] )
# 628 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "parser.mly"
                     ( _2 :: _1 )
# 636 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                                                   ( Expr(_1) )
# 643 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 99 "parser.mly"
                                                   ( Return(_2) )
# 650 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 100 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 658 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 667 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                                                   ( Print(_2) )
# 674 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
                                            ( While(_3, Block([_5])) )
# 682 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                             ( For(_3, _5, _7) )
# 691 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                  ( Noexpr )
# 697 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                  ( _1 )
# 704 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
                                   ( LitInt(_1) )
# 711 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
                                   ( LitStr(_1) )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 114 "parser.mly"
                    ( LitBool(_1) )
# 725 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_items) in
    Obj.repr(
# 115 "parser.mly"
                                   ( List(_2) )
# 732 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
                                   ( Id(_1) )
# 739 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 747 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 755 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 763 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 771 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                                   ( Binop(_1, In,       _3) )
# 779 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 787 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 795 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 803 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 811 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 819 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 827 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Assign(_1, _3) )
# 835 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                   ( Copy(_1,   _3) )
# 843 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                   ( Assign(_1, _3) )
# 851 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 131 "parser.mly"
                                   ( Call(_1,   _3) )
# 859 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathattributes) in
    Obj.repr(
# 132 "parser.mly"
                                   ( Pathattr(_1, _2) )
# 867 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
                                   ( Pathname )
# 873 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
                                   ( Pathcreated )
# 879 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
                                   ( Pathkind )
# 885 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                                   ( Item(_1) )
# 892 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'list_items) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_items) in
    Obj.repr(
# 141 "parser.mly"
                                   ( Seq(_1, Comma, _3) )
# 900 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
                    ( [] )
# 906 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 145 "parser.mly"
                    ( List.rev _1 )
# 913 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                              ( [_1] )
# 920 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                              ( _3 :: _1 )
# 928 "parser.ml"
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
