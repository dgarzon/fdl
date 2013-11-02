type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
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
  | DEF
  | VOID
  | INT
  | STR
  | DICT
  | LIST
  | PATH
  | BOOL
  | TRASH
  | LIT_INT of (int)
  | LIT_STR of (string)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 53 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRACE *);
  260 (* RBRACE *);
  261 (* LBRACK *);
  262 (* RBRACK *);
  263 (* COMMA *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIVIDE *);
  268 (* ASSIGN *);
  269 (* MOVE *);
  270 (* COPY *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* LT *);
  274 (* LEQ *);
  275 (* GT *);
  276 (* GEQ *);
  277 (* NOT *);
  278 (* AND *);
  279 (* OR *);
  280 (* CONTINUE *);
  281 (* BREAK *);
  282 (* RETURN *);
  283 (* IF *);
  284 (* THEN *);
  285 (* ELSE *);
  286 (* FOR *);
  287 (* IN *);
  288 (* WHILE *);
  289 (* DEF *);
  290 (* VOID *);
  291 (* INT *);
  292 (* STR *);
  293 (* DICT *);
  294 (* LIST *);
  295 (* PATH *);
  296 (* BOOL *);
  297 (* TRASH *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  298 (* LIT_INT *);
  299 (* LIT_STR *);
  300 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\007\000\007\000\008\000\008\000\008\000\008\000\
\008\000\008\000\005\000\005\000\002\000\002\000\002\000\002\000\
\002\000\002\000\006\000\006\000\009\000\009\000\009\000\009\000\
\011\000\011\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\010\000\010\000\010\000\010\000\
\000\000\001\000\001\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000\002\000\001\000\002\000\006\000\008\000\
\000\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

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
\000\000\000\000\000\000\004\000\000\000\000\000\035\000\036\000\
\000\000\028\000\000\000\005\000\006\000\007\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\040\000\041\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\051\000\000\000\000\000\000\000\000\000\000\000\032\000"

let yydgoto = "\002\000\
\003\000\070\000\012\000\040\000\065\000\071\000\041\000\042\000\
\082\000\083\000\000\000\106\000\107\000"

let yysindex = "\001\000\
\000\000\000\000\224\254\186\255\242\254\251\254\003\255\021\255\
\022\255\035\255\000\000\000\000\039\255\042\255\044\255\046\255\
\047\255\000\000\000\000\000\000\000\000\000\000\000\000\094\255\
\095\255\105\255\108\255\129\255\080\255\080\255\080\255\080\255\
\080\255\063\255\087\255\090\255\092\255\093\255\097\255\137\255\
\135\255\000\000\155\255\156\255\158\255\180\255\000\000\000\000\
\000\000\000\000\000\000\000\000\178\255\080\255\182\255\184\255\
\185\255\189\255\000\000\000\000\000\000\000\000\000\000\000\000\
\131\255\131\255\131\255\131\255\131\255\000\000\034\255\036\000\
\039\000\042\000\063\000\000\000\223\254\192\255\000\000\000\000\
\036\255\000\000\125\000\000\000\000\000\000\000\000\000\125\000\
\223\254\223\254\223\254\223\254\223\254\223\254\223\254\223\254\
\223\254\223\254\223\254\223\254\223\254\223\254\223\254\112\000\
\125\000\188\255\201\255\125\000\125\000\125\000\045\255\045\255\
\000\000\000\000\138\000\138\000\247\255\247\255\247\255\247\255\
\181\255\000\000\223\254\073\000\125\000\190\255\073\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\211\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\215\255\215\255\215\255\215\255\
\215\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\216\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\066\000\066\000\066\000\066\000\066\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\255\000\000\006\000\000\000\000\000\000\000\000\000\032\000\
\000\000\225\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\255\000\000\137\255\234\255\242\255\012\000\055\255\085\255\
\000\000\000\000\208\255\238\255\106\255\136\255\157\255\187\255\
\000\000\000\000\000\000\000\000\078\255\069\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\225\000\000\000\114\000\134\000\035\001\000\000\178\000\
\160\255\179\255\000\000\000\000\000\000"

let yytablesize = 414
let yytable = "\088\000\
\004\000\001\000\005\000\006\000\007\000\008\000\009\000\010\000\
\079\000\080\000\081\000\104\000\105\000\108\000\109\000\110\000\
\111\000\112\000\113\000\114\000\115\000\116\000\117\000\118\000\
\119\000\120\000\037\000\126\000\037\000\018\000\128\000\037\000\
\037\000\037\000\037\000\037\000\090\000\076\000\019\000\037\000\
\037\000\037\000\037\000\037\000\037\000\125\000\020\000\091\000\
\092\000\093\000\037\000\037\000\054\000\037\000\096\000\097\000\
\038\000\054\000\038\000\077\000\078\000\038\000\038\000\038\000\
\021\000\022\000\037\000\037\000\037\000\038\000\038\000\038\000\
\038\000\038\000\038\000\079\000\080\000\081\000\023\000\055\000\
\038\000\038\000\024\000\038\000\055\000\025\000\039\000\026\000\
\039\000\027\000\028\000\039\000\039\000\039\000\029\000\030\000\
\038\000\038\000\038\000\039\000\039\000\039\000\039\000\039\000\
\039\000\031\000\047\000\044\000\032\000\044\000\039\000\039\000\
\044\000\039\000\034\000\035\000\036\000\037\000\038\000\039\000\
\044\000\044\000\044\000\044\000\044\000\044\000\039\000\039\000\
\039\000\033\000\048\000\044\000\044\000\049\000\044\000\050\000\
\051\000\045\000\053\000\045\000\052\000\054\000\045\000\043\000\
\044\000\045\000\046\000\044\000\044\000\044\000\045\000\045\000\
\045\000\045\000\045\000\045\000\055\000\056\000\046\000\057\000\
\046\000\045\000\045\000\046\000\045\000\005\000\006\000\007\000\
\008\000\009\000\010\000\046\000\046\000\046\000\046\000\046\000\
\046\000\045\000\045\000\045\000\059\000\058\000\046\000\046\000\
\061\000\046\000\062\000\063\000\047\000\122\000\047\000\064\000\
\089\000\047\000\066\000\067\000\068\000\069\000\046\000\046\000\
\046\000\047\000\047\000\047\000\047\000\047\000\047\000\123\000\
\124\000\042\000\056\000\042\000\047\000\047\000\042\000\047\000\
\009\000\010\000\127\000\013\000\014\000\015\000\042\000\042\000\
\016\000\017\000\052\000\011\000\047\000\047\000\047\000\060\000\
\000\000\042\000\042\000\048\000\042\000\048\000\000\000\043\000\
\048\000\043\000\000\000\050\000\043\000\050\000\000\000\000\000\
\050\000\042\000\042\000\042\000\043\000\043\000\094\000\095\000\
\096\000\097\000\000\000\048\000\048\000\000\000\048\000\043\000\
\043\000\029\000\043\000\050\000\050\000\049\000\050\000\049\000\
\000\000\000\000\049\000\048\000\048\000\048\000\000\000\043\000\
\043\000\043\000\000\000\050\000\050\000\050\000\000\000\029\000\
\029\000\000\000\029\000\030\000\000\000\049\000\049\000\084\000\
\049\000\000\000\085\000\000\000\000\000\086\000\000\000\029\000\
\029\000\029\000\000\000\000\000\000\000\049\000\049\000\049\000\
\000\000\030\000\030\000\000\000\030\000\077\000\078\000\000\000\
\077\000\078\000\087\000\077\000\078\000\027\000\000\000\000\000\
\031\000\030\000\030\000\030\000\000\000\079\000\080\000\081\000\
\079\000\080\000\081\000\079\000\080\000\081\000\000\000\000\000\
\077\000\078\000\000\000\027\000\027\000\000\000\031\000\031\000\
\000\000\000\000\077\000\078\000\072\000\073\000\074\000\075\000\
\079\000\080\000\081\000\027\000\027\000\027\000\031\000\031\000\
\031\000\121\000\079\000\080\000\081\000\000\000\000\000\094\000\
\095\000\096\000\097\000\000\000\000\000\000\000\098\000\099\000\
\100\000\101\000\102\000\103\000\094\000\095\000\096\000\097\000\
\000\000\000\000\000\000\098\000\099\000\100\000\101\000\102\000\
\103\000\094\000\095\000\096\000\097\000\000\000\000\000\000\000\
\000\000\000\000\100\000\101\000\102\000\103\000"

let yycheck = "\077\000\
\033\001\001\000\035\001\036\001\037\001\038\001\039\001\040\001\
\042\001\043\001\044\001\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\002\001\124\000\004\001\044\001\127\000\007\001\
\008\001\009\001\010\001\011\001\001\001\004\001\044\001\015\001\
\016\001\017\001\018\001\019\001\020\001\123\000\044\001\012\001\
\013\001\014\001\026\001\027\001\002\001\029\001\010\001\011\001\
\002\001\007\001\004\001\026\001\027\001\007\001\008\001\009\001\
\044\001\044\001\042\001\043\001\044\001\015\001\016\001\017\001\
\018\001\019\001\020\001\042\001\043\001\044\001\044\001\002\001\
\026\001\027\001\044\001\029\001\007\001\044\001\002\001\044\001\
\004\001\044\001\044\001\007\001\008\001\009\001\001\001\001\001\
\042\001\043\001\044\001\015\001\016\001\017\001\018\001\019\001\
\020\001\001\001\044\001\002\001\001\001\004\001\026\001\027\001\
\007\001\029\001\035\001\036\001\037\001\038\001\039\001\040\001\
\015\001\016\001\017\001\018\001\019\001\020\001\042\001\043\001\
\044\001\001\001\044\001\026\001\027\001\044\001\029\001\044\001\
\044\001\002\001\002\001\004\001\044\001\007\001\007\001\030\000\
\031\000\032\000\033\000\042\001\043\001\044\001\015\001\016\001\
\017\001\018\001\019\001\020\001\002\001\002\001\002\001\002\001\
\004\001\026\001\027\001\007\001\029\001\035\001\036\001\037\001\
\038\001\039\001\040\001\015\001\016\001\017\001\018\001\019\001\
\020\001\042\001\043\001\044\001\003\001\002\001\026\001\027\001\
\003\001\029\001\003\001\003\001\002\001\002\001\004\001\003\001\
\001\001\007\001\061\000\062\000\063\000\064\000\042\001\043\001\
\044\001\015\001\016\001\017\001\018\001\019\001\020\001\007\001\
\028\001\002\001\000\000\004\001\026\001\027\001\007\001\029\001\
\002\001\002\001\029\001\034\001\035\001\036\001\015\001\016\001\
\039\001\040\001\002\001\003\000\042\001\043\001\044\001\054\000\
\255\255\026\001\027\001\002\001\029\001\004\001\255\255\002\001\
\007\001\004\001\255\255\002\001\007\001\004\001\255\255\255\255\
\007\001\042\001\043\001\044\001\015\001\016\001\008\001\009\001\
\010\001\011\001\255\255\026\001\027\001\255\255\029\001\026\001\
\027\001\004\001\029\001\026\001\027\001\002\001\029\001\004\001\
\255\255\255\255\007\001\042\001\043\001\044\001\255\255\042\001\
\043\001\044\001\255\255\042\001\043\001\044\001\255\255\026\001\
\027\001\255\255\029\001\004\001\255\255\026\001\027\001\004\001\
\029\001\255\255\004\001\255\255\255\255\004\001\255\255\042\001\
\043\001\044\001\255\255\255\255\255\255\042\001\043\001\044\001\
\255\255\026\001\027\001\255\255\029\001\026\001\027\001\255\255\
\026\001\027\001\004\001\026\001\027\001\004\001\255\255\255\255\
\004\001\042\001\043\001\044\001\255\255\042\001\043\001\044\001\
\042\001\043\001\044\001\042\001\043\001\044\001\255\255\255\255\
\026\001\027\001\255\255\026\001\027\001\255\255\026\001\027\001\
\255\255\255\255\026\001\027\001\066\000\067\000\068\000\069\000\
\042\001\043\001\044\001\042\001\043\001\044\001\042\001\043\001\
\044\001\002\001\042\001\043\001\044\001\255\255\255\255\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\008\001\009\001\010\001\011\001\255\255\255\255\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  COMMA\000\
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
  DEF\000\
  VOID\000\
  INT\000\
  STR\000\
  DICT\000\
  LIST\000\
  PATH\000\
  BOOL\000\
  TRASH\000\
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
# 351 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 32 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 359 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 33 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 367 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 37 "parser.mly"
       ({
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 381 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 43 "parser.mly"
      ({
        return = IntType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 396 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 50 "parser.mly"
      ({
        return = StrType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 411 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 57 "parser.mly"
      ({
        return = PathType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 426 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 64 "parser.mly"
      ({
        return = BoolType;
        fname = _3;
        formals = _5;
        locals = List.rev _8;
        body = List.rev _9 })
# 441 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
    ( [] )
# 447 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 73 "parser.mly"
                    ({ List.rev _1 })
# 454 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 76 "parser.mly"
                             ({ [_1] })
# 461 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 77 "parser.mly"
                               ({ _3 :: _1 })
# 469 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
                ({ VarType = IntType; name = _2; })
# 476 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
                  ({ VarType = BoolType; name = _2; })
# 483 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
                  ({ VarType = PathType; name = _2; })
# 490 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                  ({ VarType = StrType; name = _2; })
# 497 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                  ({ VarType = DictType; name = _2; })
# 504 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                  ({ VarType = ListType; name = _2; })
# 511 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
    ( [] )
# 517 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 90 "parser.mly"
                       ( _2 :: _1 )
# 525 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
              ({VarType = Ast.Int;  VarName = _2; DataType = Ast.Int})
# 532 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
                ({VarType = Ast.Bool; VarName = _2; DataType = Ast.Bool})
# 539 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parser.mly"
                ({VarType = Ast.Str;  VarName = _2; DataType = Ast.Str})
# 546 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
                ({VarType = Ast.Path; VarName = _2; DataType = Ast.Path})
# 553 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
                ({VarType = Ast.Dict; VarName = _2; DataType = Ast.Dict})
# 560 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
                ({VarType = Ast.List; VarName = _2; DataType = Ast.List})
# 567 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
    ( [] )
# 573 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "parser.mly"
                     ( _2 :: _1 )
# 581 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
         ( Expr(_1) )
# 588 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                                                   ( Return(_2) )
# 595 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 603 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 108 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 612 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
    ( Noexpr )
# 618 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                    ( _1 )
# 625 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 115 "parser.mly"
                                   ( LitInt(_1) )
# 632 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
                                   ( LitStr(_1) )
# 639 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                                   ( Id(_1) )
# 646 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 654 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 686 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 694 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 702 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 710 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 726 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Assign(_1, _3) )
# 734 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                   ( Copy(_1,   _3) )
# 742 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                   ( Assign(_1, _3) )
# 750 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 131 "parser.mly"
                                   ( Call(_1,   _3) )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
                  ( [] )
# 764 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 135 "parser.mly"
                    ( List.rev _1 )
# 771 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                            ( [_1] )
# 778 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                              ( _3 :: _1 )
# 786 "parser.ml"
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
