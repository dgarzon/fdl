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
  | LIT_INT of (int)
  | LIT_STR of (string)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 58 "parser.ml"
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
    0 (* EOF *);
    0|]

let yytransl_block = [|
  303 (* LIT_INT *);
  304 (* LIT_STR *);
  305 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\007\000\007\000\008\000\008\000\008\000\008\000\
\008\000\008\000\005\000\005\000\009\000\009\000\002\000\002\000\
\002\000\002\000\002\000\002\000\006\000\006\000\010\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\010\000\010\000\010\000\010\000\
\000\000\001\000\001\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000\001\000\001\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\000\000\002\000\002\000\003\000\
\006\000\008\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

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
\000\000\000\000\000\000\004\000\000\000\000\000\035\000\036\000\
\000\000\030\000\000\000\005\000\006\000\007\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\000\000\000\000\
\000\000\034\000"

let yydgoto = "\002\000\
\003\000\071\000\012\000\046\000\072\000\078\000\047\000\048\000\
\073\000\090\000\091\000\116\000\117\000"

let yysindex = "\005\000\
\000\000\000\000\002\000\009\000\219\254\252\254\010\255\011\255\
\018\255\024\255\000\000\000\000\030\255\033\255\037\255\047\255\
\060\255\057\255\102\255\134\255\136\255\144\255\153\255\163\255\
\165\255\186\255\187\255\188\255\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\015\000\015\000\015\000\015\000\142\255\
\166\255\167\255\169\255\171\255\181\255\191\255\207\255\000\000\
\230\255\231\255\233\255\235\255\000\000\000\000\000\000\000\000\
\000\000\000\000\228\255\015\000\240\255\254\255\255\255\007\000\
\021\000\000\000\021\000\021\000\021\000\021\000\000\000\000\000\
\021\000\000\000\000\000\000\000\000\000\003\255\000\000\006\255\
\014\255\029\255\036\255\000\000\075\255\016\000\000\000\000\000\
\140\255\000\000\229\255\000\000\000\000\000\000\000\000\243\255\
\075\255\075\255\075\255\075\255\075\255\000\000\075\255\075\255\
\075\255\075\255\075\255\075\255\075\255\075\255\075\255\075\255\
\000\000\108\255\001\000\013\000\252\255\001\000\001\000\001\000\
\017\255\017\255\000\000\000\000\014\000\014\000\026\255\026\255\
\026\255\026\255\242\255\000\000\075\255\046\255\001\000\006\000\
\046\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\028\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\027\000\027\000\027\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\040\255\000\000\040\255\040\255\040\255\040\255\000\000\000\000\
\043\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\095\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\255\000\000\030\000\002\255\039\255\135\255\
\129\255\150\255\000\000\000\000\210\255\227\255\156\255\177\255\
\183\255\204\255\000\000\000\000\000\000\000\000\096\255\052\255\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\000\000\096\000\113\000\133\000\000\000\235\000\
\000\000\124\255\172\255\000\000\000\000"

let yytablesize = 320
let yytable = "\011\000\
\096\000\136\000\054\000\048\000\138\000\001\000\084\000\054\000\
\048\000\092\000\048\000\018\000\114\000\115\000\118\000\119\000\
\120\000\093\000\121\000\122\000\123\000\124\000\125\000\126\000\
\127\000\128\000\129\000\130\000\105\000\106\000\085\000\086\000\
\094\000\085\000\086\000\103\000\104\000\105\000\106\000\095\000\
\050\000\085\000\086\000\019\000\019\000\050\000\020\000\050\000\
\135\000\087\000\088\000\089\000\087\000\088\000\089\000\033\000\
\085\000\086\000\020\000\021\000\087\000\088\000\089\000\085\000\
\086\000\029\000\022\000\019\000\019\000\079\000\020\000\020\000\
\023\000\085\000\086\000\087\000\088\000\089\000\024\000\033\000\
\033\000\025\000\087\000\088\000\089\000\026\000\019\000\019\000\
\019\000\020\000\020\000\020\000\087\000\088\000\089\000\027\000\
\037\000\055\000\033\000\033\000\033\000\037\000\055\000\037\000\
\037\000\037\000\037\000\037\000\028\000\131\000\030\000\037\000\
\037\000\037\000\037\000\037\000\037\000\103\000\104\000\105\000\
\106\000\087\000\088\000\089\000\107\000\108\000\109\000\110\000\
\111\000\112\000\038\000\049\000\050\000\051\000\052\000\038\000\
\049\000\038\000\038\000\038\000\098\000\049\000\031\000\049\000\
\032\000\038\000\038\000\038\000\038\000\038\000\038\000\039\000\
\033\000\099\000\100\000\101\000\039\000\044\000\039\000\039\000\
\039\000\034\000\044\000\035\000\044\000\036\000\039\000\039\000\
\039\000\039\000\039\000\039\000\044\000\044\000\044\000\044\000\
\044\000\044\000\045\000\074\000\075\000\076\000\077\000\045\000\
\046\000\045\000\037\000\038\000\039\000\046\000\053\000\046\000\
\059\000\045\000\045\000\045\000\045\000\045\000\045\000\046\000\
\046\000\046\000\046\000\046\000\046\000\047\000\080\000\081\000\
\082\000\083\000\047\000\042\000\047\000\060\000\054\000\055\000\
\042\000\056\000\042\000\057\000\047\000\047\000\047\000\047\000\
\047\000\047\000\042\000\042\000\043\000\058\000\065\000\061\000\
\062\000\043\000\063\000\043\000\064\000\102\000\103\000\104\000\
\105\000\106\000\067\000\043\000\043\000\107\000\108\000\109\000\
\110\000\111\000\112\000\113\000\103\000\104\000\105\000\106\000\
\068\000\069\000\133\000\107\000\108\000\109\000\110\000\111\000\
\112\000\070\000\103\000\104\000\105\000\106\000\132\000\134\000\
\097\000\107\000\108\000\109\000\110\000\111\000\112\000\103\000\
\104\000\105\000\106\000\056\000\009\000\010\000\052\000\053\000\
\109\000\110\000\111\000\112\000\137\000\004\000\066\000\005\000\
\006\000\007\000\008\000\009\000\010\000\013\000\014\000\015\000\
\000\000\000\000\016\000\017\000\040\000\041\000\042\000\043\000\
\044\000\045\000\005\000\006\000\007\000\008\000\009\000\010\000"

let yycheck = "\003\000\
\085\000\134\000\002\001\002\001\137\000\001\000\004\001\007\001\
\007\001\004\001\009\001\049\001\097\000\098\000\099\000\100\000\
\101\000\004\001\103\000\104\000\105\000\106\000\107\000\108\000\
\109\000\110\000\111\000\112\000\012\001\013\001\028\001\029\001\
\004\001\028\001\029\001\010\001\011\001\012\001\013\001\004\001\
\002\001\028\001\029\001\004\001\049\001\007\001\004\001\009\001\
\133\000\047\001\048\001\049\001\047\001\048\001\049\001\004\001\
\028\001\029\001\049\001\049\001\047\001\048\001\049\001\028\001\
\029\001\009\001\049\001\028\001\029\001\073\000\028\001\029\001\
\049\001\028\001\029\001\047\001\048\001\049\001\049\001\028\001\
\029\001\049\001\047\001\048\001\049\001\049\001\047\001\048\001\
\049\001\047\001\048\001\049\001\047\001\048\001\049\001\049\001\
\002\001\002\001\047\001\048\001\049\001\007\001\007\001\009\001\
\010\001\011\001\012\001\013\001\049\001\002\001\009\001\017\001\
\018\001\019\001\020\001\021\001\022\001\010\001\011\001\012\001\
\013\001\047\001\048\001\049\001\017\001\018\001\019\001\020\001\
\021\001\022\001\002\001\036\000\037\000\038\000\039\000\007\001\
\002\001\009\001\010\001\011\001\001\001\007\001\009\001\009\001\
\009\001\017\001\018\001\019\001\020\001\021\001\022\001\002\001\
\009\001\014\001\015\001\016\001\007\001\002\001\009\001\010\001\
\011\001\009\001\007\001\001\001\009\001\001\001\017\001\018\001\
\019\001\020\001\021\001\022\001\017\001\018\001\019\001\020\001\
\021\001\022\001\002\001\067\000\068\000\069\000\070\000\007\001\
\002\001\009\001\001\001\001\001\001\001\007\001\049\001\009\001\
\002\001\017\001\018\001\019\001\020\001\021\001\022\001\017\001\
\018\001\019\001\020\001\021\001\022\001\002\001\074\000\075\000\
\076\000\077\000\007\001\002\001\009\001\007\001\049\001\049\001\
\007\001\049\001\009\001\049\001\017\001\018\001\019\001\020\001\
\021\001\022\001\017\001\018\001\002\001\049\001\003\001\002\001\
\002\001\007\001\002\001\009\001\002\001\009\001\010\001\011\001\
\012\001\013\001\003\001\017\001\018\001\017\001\018\001\019\001\
\020\001\021\001\022\001\009\001\010\001\011\001\012\001\013\001\
\003\001\003\001\007\001\017\001\018\001\019\001\020\001\021\001\
\022\001\003\001\010\001\011\001\012\001\013\001\002\001\030\001\
\001\001\017\001\018\001\019\001\020\001\021\001\022\001\010\001\
\011\001\012\001\013\001\000\000\002\001\002\001\002\001\002\001\
\019\001\020\001\021\001\022\001\031\001\036\001\060\000\038\001\
\039\001\040\001\041\001\042\001\043\001\037\001\038\001\039\001\
\255\255\255\255\042\001\043\001\038\001\039\001\040\001\041\001\
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
# 348 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 32 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 356 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 33 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 364 "parser.ml"
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
# 379 "parser.ml"
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
# 394 "parser.ml"
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
# 409 "parser.ml"
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
# 424 "parser.ml"
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
# 439 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
    ( [] )
# 445 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 76 "parser.mly"
                    ( List.rev _1 )
# 452 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 79 "parser.mly"
                               ( [_1] )
# 459 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 80 "parser.mly"
                               ( _3 :: _1 )
# 467 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                  ( { vtype = IntType;  vname = _2; } )
# 474 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                  ( { vtype = BoolType; vname = _2; } )
# 481 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                  ( { vtype = PathType; vname = _2; } )
# 488 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                  ( { vtype = StrType;  vname = _2; } )
# 495 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                  ( { vtype = DictType; vname = _2; } )
# 502 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                  ( { vtype = ListType; vname = _2; } )
# 509 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
    ( [] )
# 515 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 93 "parser.mly"
                    ( List.rev _1 )
# 522 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 96 "parser.mly"
                       ( [_1] )
# 529 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 97 "parser.mly"
                       ( _2 :: _1 )
# 537 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 101 "parser.mly"
                   ( { vtype = IntType;  vname = _2; } )
# 544 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 102 "parser.mly"
                   ( { vtype = BoolType; vname = _2; } )
# 551 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 103 "parser.mly"
                   ( { vtype = StrType;  vname = _2; } )
# 558 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 104 "parser.mly"
                   ( { vtype = PathType; vname = _2; } )
# 565 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 105 "parser.mly"
                   ( { vtype = DictType; vname = _2; } )
# 572 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 106 "parser.mly"
                   ( { vtype = ListType; vname = _2; } )
# 579 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
    ( [] )
# 585 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 110 "parser.mly"
                     ( _2 :: _1 )
# 593 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                                                   ( Expr(_1) )
# 600 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                                                   ( Return(_2) )
# 607 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 116 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 615 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 624 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 120 "parser.mly"
                                   ( LitInt(_1) )
# 631 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
                                   ( LitStr(_1) )
# 638 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
                                   ( Id(_1) )
# 645 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 653 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 661 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 685 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 693 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 701 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 709 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 717 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 725 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                                   ( Assign(_1, _3) )
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                                   ( Copy(_1,   _3) )
# 741 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                                   ( Assign(_1, _3) )
# 749 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 136 "parser.mly"
                                   ( Call(_1,   _3) )
# 757 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
                    ( [] )
# 763 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 140 "parser.mly"
                    ( List.rev _1 )
# 770 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                              ( [_1] )
# 777 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                              ( _3 :: _1 )
# 785 "parser.ml"
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
