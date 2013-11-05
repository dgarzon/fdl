type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | TAB
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
# 57 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRACE *);
  260 (* RBRACE *);
  261 (* LBRACK *);
  262 (* RBRACK *);
  263 (* COMMA *);
  264 (* TAB *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* ASSIGN *);
  270 (* MOVE *);
  271 (* COPY *);
  272 (* EQ *);
  273 (* NEQ *);
  274 (* LT *);
  275 (* LEQ *);
  276 (* GT *);
  277 (* GEQ *);
  278 (* NOT *);
  279 (* AND *);
  280 (* OR *);
  281 (* CONTINUE *);
  282 (* BREAK *);
  283 (* RETURN *);
  284 (* IF *);
  285 (* THEN *);
  286 (* ELSE *);
  287 (* FOR *);
  288 (* IN *);
  289 (* WHILE *);
  290 (* DO *);
  291 (* DEF *);
  292 (* VOID *);
  293 (* INT *);
  294 (* STR *);
  295 (* DICT *);
  296 (* LIST *);
  297 (* PATH *);
  298 (* BOOL *);
  299 (* TRASH *);
  300 (* TRUE *);
  301 (* FALSE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  302 (* LIT_INT *);
  303 (* LIT_STR *);
  304 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\007\000\007\000\008\000\008\000\008\000\008\000\
\008\000\008\000\005\000\005\000\002\000\002\000\002\000\002\000\
\002\000\002\000\006\000\006\000\009\000\009\000\009\000\009\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\011\000\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\010\000\010\000\010\000\010\000\
\000\000\001\000\001\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000\002\000\002\000\003\000\006\000\008\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\003\000\000\000\000\000\000\000\000\000\
\000\000\021\000\023\000\025\000\026\000\024\000\022\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\000\000\000\000\013\000\016\000\
\017\000\018\000\015\000\014\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\012\000\019\000\019\000\019\000\019\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\004\000\000\000\000\000\033\000\034\000\
\000\000\028\000\000\000\005\000\006\000\007\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\049\000\000\000\000\000\000\000\000\000\
\000\000\032\000"

let yydgoto = "\002\000\
\003\000\070\000\012\000\040\000\065\000\071\000\041\000\042\000\
\082\000\083\000\108\000\109\000"

let yysindex = "\010\000\
\000\000\000\000\245\255\223\254\210\254\254\254\009\255\010\255\
\022\255\029\255\000\000\000\000\032\255\056\255\057\255\069\255\
\070\255\000\000\000\000\000\000\000\000\000\000\000\000\046\255\
\156\255\157\255\158\255\166\255\251\255\251\255\251\255\251\255\
\251\255\120\255\122\255\127\255\128\255\129\255\138\255\117\255\
\206\255\000\000\219\255\221\255\222\255\231\255\000\000\000\000\
\000\000\000\000\000\000\000\000\240\255\251\255\241\255\242\255\
\254\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\002\000\002\000\002\000\002\000\000\000\003\255\006\255\
\013\255\028\255\035\255\000\000\245\254\013\000\000\000\000\000\
\134\255\000\000\230\255\000\000\000\000\000\000\000\000\244\255\
\245\254\245\254\245\254\245\254\245\254\000\000\245\254\245\254\
\245\254\245\254\245\254\245\254\245\254\245\254\245\254\245\254\
\000\000\104\255\001\000\014\000\252\255\001\000\001\000\001\000\
\017\255\017\255\000\000\000\000\118\255\118\255\085\255\085\255\
\085\255\085\255\250\255\000\000\245\254\044\255\001\000\008\000\
\044\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\015\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\023\000\023\000\023\000\023\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\255\038\255\038\255\038\255\038\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\091\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\255\254\000\000\045\000\036\255\071\255\148\255\
\124\255\144\255\000\000\000\000\218\255\220\255\164\255\171\255\
\191\255\198\255\000\000\000\000\000\000\000\000\167\255\041\255\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\045\001\000\000\164\000\140\000\163\000\000\000\251\000\
\194\255\179\255\000\000\000\000"

let yytablesize = 305
let yytable = "\088\000\
\052\000\018\000\013\000\014\000\015\000\052\000\076\000\016\000\
\017\000\084\000\001\000\106\000\107\000\110\000\111\000\112\000\
\085\000\113\000\114\000\115\000\116\000\117\000\118\000\119\000\
\120\000\121\000\122\000\097\000\098\000\077\000\078\000\086\000\
\077\000\078\000\079\000\080\000\081\000\046\000\087\000\077\000\
\078\000\027\000\046\000\046\000\031\000\019\000\029\000\127\000\
\079\000\080\000\081\000\079\000\080\000\081\000\077\000\078\000\
\020\000\021\000\079\000\080\000\081\000\077\000\078\000\128\000\
\027\000\027\000\130\000\031\000\031\000\022\000\077\000\078\000\
\048\000\079\000\080\000\081\000\023\000\048\000\048\000\024\000\
\079\000\080\000\081\000\027\000\027\000\027\000\031\000\031\000\
\031\000\079\000\080\000\081\000\035\000\095\000\096\000\097\000\
\098\000\035\000\035\000\035\000\035\000\035\000\035\000\025\000\
\026\000\123\000\035\000\035\000\035\000\035\000\035\000\035\000\
\095\000\096\000\097\000\098\000\027\000\028\000\053\000\099\000\
\100\000\101\000\102\000\103\000\104\000\036\000\095\000\096\000\
\097\000\098\000\036\000\036\000\036\000\036\000\090\000\101\000\
\102\000\103\000\104\000\036\000\036\000\036\000\036\000\036\000\
\036\000\037\000\091\000\092\000\093\000\047\000\037\000\037\000\
\037\000\037\000\047\000\047\000\030\000\031\000\032\000\037\000\
\037\000\037\000\037\000\037\000\037\000\042\000\033\000\047\000\
\053\000\048\000\042\000\042\000\043\000\053\000\049\000\050\000\
\051\000\043\000\043\000\042\000\042\000\042\000\042\000\042\000\
\042\000\052\000\043\000\043\000\043\000\043\000\043\000\043\000\
\044\000\043\000\044\000\045\000\046\000\044\000\044\000\045\000\
\066\000\067\000\068\000\069\000\045\000\045\000\044\000\044\000\
\044\000\044\000\044\000\044\000\054\000\045\000\045\000\045\000\
\045\000\045\000\045\000\040\000\055\000\041\000\056\000\057\000\
\040\000\040\000\041\000\041\000\072\000\073\000\074\000\075\000\
\058\000\040\000\040\000\041\000\041\000\094\000\095\000\096\000\
\097\000\098\000\059\000\061\000\062\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\095\000\096\000\097\000\098\000\
\063\000\064\000\125\000\099\000\100\000\101\000\102\000\103\000\
\104\000\095\000\096\000\097\000\098\000\089\000\054\000\124\000\
\099\000\100\000\101\000\102\000\103\000\104\000\126\000\004\000\
\009\000\005\000\006\000\007\000\008\000\009\000\010\000\034\000\
\035\000\036\000\037\000\038\000\039\000\129\000\005\000\006\000\
\007\000\008\000\009\000\010\000\010\000\050\000\051\000\011\000\
\060\000"

let yycheck = "\077\000\
\002\001\048\001\036\001\037\001\038\001\007\001\004\001\041\001\
\042\001\004\001\001\000\089\000\090\000\091\000\092\000\093\000\
\004\001\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\011\001\012\001\027\001\028\001\004\001\
\027\001\028\001\046\001\047\001\048\001\002\001\004\001\027\001\
\028\001\004\001\007\001\008\001\004\001\048\001\001\001\125\000\
\046\001\047\001\048\001\046\001\047\001\048\001\027\001\028\001\
\048\001\048\001\046\001\047\001\048\001\027\001\028\001\126\000\
\027\001\028\001\129\000\027\001\028\001\048\001\027\001\028\001\
\002\001\046\001\047\001\048\001\048\001\007\001\008\001\048\001\
\046\001\047\001\048\001\046\001\047\001\048\001\046\001\047\001\
\048\001\046\001\047\001\048\001\002\001\009\001\010\001\011\001\
\012\001\007\001\008\001\009\001\010\001\011\001\012\001\048\001\
\048\001\002\001\016\001\017\001\018\001\019\001\020\001\021\001\
\009\001\010\001\011\001\012\001\048\001\048\001\002\001\016\001\
\017\001\018\001\019\001\020\001\021\001\002\001\009\001\010\001\
\011\001\012\001\007\001\008\001\009\001\010\001\001\001\018\001\
\019\001\020\001\021\001\016\001\017\001\018\001\019\001\020\001\
\021\001\002\001\013\001\014\001\015\001\002\001\007\001\008\001\
\009\001\010\001\007\001\008\001\001\001\001\001\001\001\016\001\
\017\001\018\001\019\001\020\001\021\001\002\001\001\001\048\001\
\002\001\048\001\007\001\008\001\002\001\007\001\048\001\048\001\
\048\001\007\001\008\001\016\001\017\001\018\001\019\001\020\001\
\021\001\048\001\016\001\017\001\018\001\019\001\020\001\021\001\
\002\001\030\000\031\000\032\000\033\000\007\001\008\001\002\001\
\061\000\062\000\063\000\064\000\007\001\008\001\016\001\017\001\
\018\001\019\001\020\001\021\001\007\001\016\001\017\001\018\001\
\019\001\020\001\021\001\002\001\002\001\002\001\002\001\002\001\
\007\001\008\001\007\001\008\001\066\000\067\000\068\000\069\000\
\002\001\016\001\017\001\016\001\017\001\008\001\009\001\010\001\
\011\001\012\001\003\001\003\001\003\001\016\001\017\001\018\001\
\019\001\020\001\021\001\008\001\009\001\010\001\011\001\012\001\
\003\001\003\001\007\001\016\001\017\001\018\001\019\001\020\001\
\021\001\009\001\010\001\011\001\012\001\001\001\000\000\002\001\
\016\001\017\001\018\001\019\001\020\001\021\001\029\001\035\001\
\002\001\037\001\038\001\039\001\040\001\041\001\042\001\037\001\
\038\001\039\001\040\001\041\001\042\001\030\001\037\001\038\001\
\039\001\040\001\041\001\042\001\002\001\002\001\002\001\003\000\
\054\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  COMMA\000\
  TAB\000\
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
# 340 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 32 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 348 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 33 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 356 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 37 "parser.mly"
       ({
        return = VoidType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 371 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 44 "parser.mly"
      ({
        return = IntType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 386 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 51 "parser.mly"
      ({
        return = StrType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 401 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 58 "parser.mly"
      ({
        return = PathType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 416 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 65 "parser.mly"
      ({
        return = BoolType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 431 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
    ( [] )
# 437 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 76 "parser.mly"
                    ( List.rev _1 )
# 444 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 79 "parser.mly"
                             ( [_1] )
# 451 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 80 "parser.mly"
                               ( _3 :: _1 )
# 459 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                ({ vtype = IntType; vname = _2; })
# 466 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                  ({ vtype = BoolType; vname = _2; })
# 473 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                  ({ vtype = PathType; vname = _2; })
# 480 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                  ({ vtype = StrType; vname = _2; })
# 487 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                  ({ vtype = DictType; vname = _2; })
# 494 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                  ({ vtype = ListType; vname = _2; })
# 501 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
    ( [] )
# 507 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 93 "parser.mly"
                       ( _2 :: _1 )
# 515 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
              ({ vtype = IntType;  vname = _2; })
# 522 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
                ({ vtype = BoolType; vname = _2; })
# 529 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
                ({ vtype = StrType;  vname = _2; })
# 536 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "parser.mly"
                ({ vtype = PathType; vname = _2; })
# 543 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
                ({ vtype = DictType; vname = _2; })
# 550 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "parser.mly"
                ({ vtype = ListType; vname = _2; })
# 557 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
    ( [] )
# 563 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                     ( _2 :: _1 )
# 571 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
             ( Expr(_1) )
# 578 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                                                     ( Return(_2) )
# 585 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 110 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 593 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 111 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 602 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 114 "parser.mly"
                                   ( LitInt(_1) )
# 609 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
                                   ( LitStr(_1) )
# 616 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
                                   ( Id(_1) )
# 623 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 631 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 639 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 647 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 663 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 671 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 679 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 687 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 695 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 703 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                                   ( Assign(_1, _3) )
# 711 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Copy(_1,   _3) )
# 719 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                   ( Assign(_1, _3) )
# 727 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 130 "parser.mly"
                                   ( Call(_1,   _3) )
# 735 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                  ( [] )
# 741 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 134 "parser.mly"
                    ( List.rev _1 )
# 748 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                            ( [_1] )
# 755 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                              ( _3 :: _1 )
# 763 "parser.ml"
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
