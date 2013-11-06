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
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\007\000\007\000\008\000\008\000\008\000\008\000\
\008\000\008\000\005\000\005\000\009\000\009\000\002\000\002\000\
\002\000\002\000\002\000\002\000\006\000\006\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\012\000\012\000\013\000\013\000\
\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\010\000\010\000\010\000\010\000\
\000\000\001\000\001\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000\001\000\001\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\000\000\002\000\002\000\003\000\
\006\000\008\000\003\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\000\000\001\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\003\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\023\000\025\000\027\000\028\000\
\026\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\000\000\000\000\000\000\000\000\013\000\016\000\017\000\018\000\
\015\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\021\000\029\000\
\000\000\029\000\029\000\029\000\029\000\000\000\022\000\000\000\
\000\000\000\000\000\000\004\000\000\000\000\000\000\000\036\000\
\037\000\000\000\030\000\000\000\005\000\006\000\007\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\032\000\000\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\041\000\042\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\052\000\000\000\
\000\000\000\000\000\000\000\000\034\000"

let yydgoto = "\002\000\
\003\000\071\000\012\000\046\000\072\000\078\000\047\000\048\000\
\073\000\091\000\092\000\119\000\120\000"

let yysindex = "\032\000\
\000\000\000\000\222\254\038\000\247\254\248\254\252\254\000\255\
\012\255\019\255\000\000\000\000\027\255\033\255\035\255\064\255\
\066\255\112\255\119\255\125\255\127\255\138\255\139\255\148\255\
\161\255\177\255\186\255\188\255\000\000\000\000\000\000\000\000\
\000\000\000\000\044\000\044\000\044\000\044\000\044\000\141\255\
\162\255\163\255\164\255\166\255\168\255\230\255\226\255\000\000\
\232\255\233\255\237\255\238\255\000\000\000\000\000\000\000\000\
\000\000\000\000\240\255\044\000\242\255\255\255\001\000\003\000\
\050\000\000\000\050\000\050\000\050\000\050\000\000\000\000\000\
\050\000\000\000\000\000\000\000\000\000\006\255\000\000\011\255\
\016\255\039\255\043\255\000\000\219\254\011\000\219\254\000\000\
\000\000\165\255\000\000\254\255\000\000\000\000\000\000\000\000\
\012\000\219\254\026\000\219\254\219\254\219\254\219\254\000\000\
\219\254\219\254\219\254\219\254\219\254\219\254\219\254\219\254\
\219\254\219\254\000\000\133\255\000\000\039\000\239\255\248\255\
\039\000\039\000\039\000\036\255\036\255\000\000\000\000\052\000\
\052\000\097\255\097\255\097\255\097\255\227\255\000\000\219\254\
\076\255\039\000\225\255\076\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\026\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\025\000\025\000\025\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\255\000\000\047\255\047\255\047\255\047\255\000\000\000\000\
\053\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\120\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\255\000\000\053\000\
\029\255\071\255\104\255\154\255\175\255\000\000\000\000\235\255\
\252\255\181\255\202\255\208\255\229\255\000\000\000\000\000\000\
\000\000\077\255\070\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\000\000\121\000\100\000\131\000\000\000\224\000\
\000\000\231\255\172\255\000\000\000\000"

let yytablesize = 349
let yytable = "\011\000\
\097\000\004\000\099\000\005\000\006\000\007\000\008\000\009\000\
\010\000\084\000\088\000\089\000\090\000\116\000\093\000\118\000\
\121\000\122\000\123\000\094\000\124\000\125\000\126\000\127\000\
\128\000\129\000\130\000\131\000\132\000\133\000\049\000\055\000\
\001\000\085\000\086\000\049\000\055\000\049\000\085\000\086\000\
\018\000\019\000\095\000\085\000\086\000\020\000\096\000\107\000\
\108\000\021\000\019\000\138\000\087\000\088\000\089\000\090\000\
\020\000\087\000\088\000\089\000\090\000\022\000\087\000\088\000\
\089\000\090\000\085\000\086\000\023\000\079\000\085\000\086\000\
\051\000\033\000\019\000\019\000\024\000\051\000\056\000\051\000\
\020\000\020\000\025\000\056\000\026\000\087\000\088\000\089\000\
\090\000\087\000\088\000\089\000\090\000\019\000\019\000\019\000\
\019\000\033\000\033\000\020\000\020\000\020\000\020\000\085\000\
\086\000\050\000\105\000\106\000\107\000\108\000\050\000\139\000\
\050\000\027\000\141\000\028\000\033\000\033\000\033\000\033\000\
\029\000\038\000\087\000\088\000\089\000\090\000\038\000\030\000\
\038\000\038\000\038\000\038\000\038\000\031\000\134\000\032\000\
\038\000\038\000\038\000\038\000\038\000\038\000\105\000\106\000\
\107\000\108\000\033\000\034\000\035\000\109\000\110\000\111\000\
\112\000\113\000\114\000\039\000\049\000\050\000\051\000\052\000\
\039\000\036\000\039\000\039\000\039\000\100\000\074\000\075\000\
\076\000\077\000\039\000\039\000\039\000\039\000\039\000\039\000\
\040\000\037\000\101\000\102\000\103\000\040\000\045\000\040\000\
\040\000\040\000\038\000\045\000\039\000\045\000\053\000\040\000\
\040\000\040\000\040\000\040\000\040\000\045\000\045\000\045\000\
\045\000\045\000\045\000\046\000\080\000\081\000\082\000\083\000\
\046\000\047\000\046\000\054\000\055\000\056\000\047\000\057\000\
\047\000\058\000\046\000\046\000\046\000\046\000\046\000\046\000\
\047\000\047\000\047\000\047\000\047\000\047\000\048\000\059\000\
\060\000\061\000\062\000\048\000\043\000\048\000\063\000\064\000\
\135\000\043\000\065\000\043\000\067\000\048\000\048\000\048\000\
\048\000\048\000\048\000\043\000\043\000\044\000\136\000\140\000\
\137\000\068\000\044\000\069\000\044\000\070\000\104\000\105\000\
\106\000\107\000\108\000\098\000\044\000\044\000\109\000\110\000\
\111\000\112\000\113\000\114\000\115\000\105\000\106\000\107\000\
\108\000\057\000\009\000\066\000\109\000\110\000\111\000\112\000\
\113\000\114\000\117\000\105\000\106\000\107\000\108\000\000\000\
\000\000\010\000\109\000\110\000\111\000\112\000\113\000\114\000\
\105\000\106\000\107\000\108\000\053\000\000\000\054\000\109\000\
\110\000\111\000\112\000\113\000\114\000\105\000\106\000\107\000\
\108\000\000\000\000\000\000\000\000\000\000\000\111\000\112\000\
\113\000\114\000\013\000\014\000\015\000\000\000\000\000\016\000\
\017\000\040\000\041\000\042\000\043\000\044\000\045\000\005\000\
\006\000\007\000\008\000\009\000\010\000"

let yycheck = "\003\000\
\085\000\036\001\087\000\038\001\039\001\040\001\041\001\042\001\
\043\001\004\001\048\001\049\001\050\001\098\000\004\001\100\000\
\101\000\102\000\103\000\004\001\105\000\106\000\107\000\108\000\
\109\000\110\000\111\000\112\000\113\000\114\000\002\001\002\001\
\001\000\028\001\029\001\007\001\007\001\009\001\028\001\029\001\
\050\001\050\001\004\001\028\001\029\001\050\001\004\001\012\001\
\013\001\050\001\004\001\136\000\047\001\048\001\049\001\050\001\
\004\001\047\001\048\001\049\001\050\001\050\001\047\001\048\001\
\049\001\050\001\028\001\029\001\050\001\073\000\028\001\029\001\
\002\001\004\001\028\001\029\001\050\001\007\001\002\001\009\001\
\028\001\029\001\050\001\007\001\050\001\047\001\048\001\049\001\
\050\001\047\001\048\001\049\001\050\001\047\001\048\001\049\001\
\050\001\028\001\029\001\047\001\048\001\049\001\050\001\028\001\
\029\001\002\001\010\001\011\001\012\001\013\001\007\001\137\000\
\009\001\050\001\140\000\050\001\047\001\048\001\049\001\050\001\
\009\001\002\001\047\001\048\001\049\001\050\001\007\001\009\001\
\009\001\010\001\011\001\012\001\013\001\009\001\002\001\009\001\
\017\001\018\001\019\001\020\001\021\001\022\001\010\001\011\001\
\012\001\013\001\009\001\009\001\001\001\017\001\018\001\019\001\
\020\001\021\001\022\001\002\001\036\000\037\000\038\000\039\000\
\007\001\001\001\009\001\010\001\011\001\001\001\067\000\068\000\
\069\000\070\000\017\001\018\001\019\001\020\001\021\001\022\001\
\002\001\001\001\014\001\015\001\016\001\007\001\002\001\009\001\
\010\001\011\001\001\001\007\001\001\001\009\001\050\001\017\001\
\018\001\019\001\020\001\021\001\022\001\017\001\018\001\019\001\
\020\001\021\001\022\001\002\001\074\000\075\000\076\000\077\000\
\007\001\002\001\009\001\050\001\050\001\050\001\007\001\050\001\
\009\001\050\001\017\001\018\001\019\001\020\001\021\001\022\001\
\017\001\018\001\019\001\020\001\021\001\022\001\002\001\002\001\
\007\001\002\001\002\001\007\001\002\001\009\001\002\001\002\001\
\002\001\007\001\003\001\009\001\003\001\017\001\018\001\019\001\
\020\001\021\001\022\001\017\001\018\001\002\001\007\001\031\001\
\030\001\003\001\007\001\003\001\009\001\003\001\009\001\010\001\
\011\001\012\001\013\001\001\001\017\001\018\001\017\001\018\001\
\019\001\020\001\021\001\022\001\009\001\010\001\011\001\012\001\
\013\001\000\000\002\001\060\000\017\001\018\001\019\001\020\001\
\021\001\022\001\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\002\001\017\001\018\001\019\001\020\001\021\001\022\001\
\010\001\011\001\012\001\013\001\002\001\255\255\002\001\017\001\
\018\001\019\001\020\001\021\001\022\001\010\001\011\001\012\001\
\013\001\255\255\255\255\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\037\001\038\001\039\001\255\255\255\255\042\001\
\043\001\038\001\039\001\040\001\041\001\042\001\043\001\038\001\
\039\001\040\001\041\001\042\001\043\001"

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
# 361 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 32 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 369 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 33 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 377 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 37 "parser.mly"
       ({
        return = VoidType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 392 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 44 "parser.mly"
      ({
        return = IntType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 407 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 51 "parser.mly"
      ({
        return = StrType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 422 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 58 "parser.mly"
      ({
        return = PathType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 437 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 65 "parser.mly"
      ({
        return = BoolType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 452 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
    ( [] )
# 458 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 76 "parser.mly"
                    ( List.rev _1 )
# 465 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 79 "parser.mly"
                               ( [_1] )
# 472 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 80 "parser.mly"
                               ( _3 :: _1 )
# 480 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                  ( { vtype = IntType;  vname = _2; } )
# 487 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                  ( { vtype = BoolType; vname = _2; } )
# 494 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                  ( { vtype = PathType; vname = _2; } )
# 501 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                  ( { vtype = StrType;  vname = _2; } )
# 508 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                  ( { vtype = DictType; vname = _2; } )
# 515 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                  ( { vtype = ListType; vname = _2; } )
# 522 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
    ( [] )
# 528 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 93 "parser.mly"
                    ( List.rev _1 )
# 535 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 96 "parser.mly"
                       ( [_1] )
# 542 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 97 "parser.mly"
                       ( _2 :: _1 )
# 550 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 101 "parser.mly"
                   ( { vtype = IntType;  vname = _2; } )
# 557 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 102 "parser.mly"
                   ( { vtype = BoolType; vname = _2; } )
# 564 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 103 "parser.mly"
                   ( { vtype = StrType;  vname = _2; } )
# 571 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 104 "parser.mly"
                   ( { vtype = PathType; vname = _2; } )
# 578 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 105 "parser.mly"
                   ( { vtype = DictType; vname = _2; } )
# 585 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 106 "parser.mly"
                   ( { vtype = ListType; vname = _2; } )
# 592 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
    ( [] )
# 598 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 110 "parser.mly"
                     ( _2 :: _1 )
# 606 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                                                   ( Expr(_1) )
# 613 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                                                   ( Return(_2) )
# 620 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 116 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 628 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 637 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                                   ( Print(_2) )
# 644 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 121 "parser.mly"
                                   ( LitInt(_1) )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
                                   ( LitStr(_1) )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "parser.mly"
                                   ( Id(_1) )
# 665 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                                   ( Assign(_1, _3) )
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                                   ( Copy(_1,   _3) )
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                                   ( Assign(_1, _3) )
# 769 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 137 "parser.mly"
                                   ( Call(_1,   _3) )
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                    ( [] )
# 783 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 141 "parser.mly"
                    ( List.rev _1 )
# 790 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                              ( [_1] )
# 797 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                              ( _3 :: _1 )
# 805 "parser.ml"
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
