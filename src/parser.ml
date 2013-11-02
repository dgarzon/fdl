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
\009\000\011\000\011\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\012\000\012\000\013\000\013\000\
\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\009\000\009\000\009\000\009\000\009\000\
\000\000\001\000\001\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000\002\000\001\000\002\000\003\000\006\000\
\008\000\000\000\001\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\000\000\001\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\003\000\000\000\000\000\000\000\025\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\000\000\000\000\000\000\000\000\013\000\016\000\017\000\
\018\000\015\000\014\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\012\000\019\000\019\000\019\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\000\000\000\000\000\000\000\000\000\000\021\000\023\000\
\024\000\022\000\027\000\004\000\000\000\000\000\036\000\037\000\
\000\000\028\000\000\000\005\000\006\000\007\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\041\000\042\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\052\000\000\000\000\000\000\000\000\000\
\000\000\033\000"

let yydgoto = "\002\000\
\003\000\065\000\012\000\031\000\056\000\066\000\032\000\033\000\
\082\000\083\000\000\000\108\000\109\000"

let yysindex = "\040\000\
\000\000\000\000\044\000\254\254\006\255\015\255\017\255\020\255\
\022\255\023\255\000\000\000\000\075\255\076\255\089\255\000\000\
\000\000\090\255\093\255\091\255\091\255\091\255\091\255\091\255\
\052\255\053\255\057\255\058\255\062\255\063\255\115\255\111\255\
\000\000\139\255\140\255\143\255\145\255\000\000\000\000\000\000\
\000\000\000\000\000\000\149\255\091\255\150\255\165\255\166\255\
\190\255\000\000\000\000\000\000\000\000\000\000\000\000\142\255\
\142\255\142\255\142\255\142\255\104\255\148\255\152\255\154\255\
\000\000\045\255\240\255\109\000\115\000\122\000\000\000\000\000\
\000\000\000\000\000\000\000\000\231\254\202\255\000\000\000\000\
\255\254\000\000\207\000\000\000\000\000\000\000\000\000\141\000\
\207\000\231\254\231\254\231\254\231\254\231\254\231\254\231\254\
\231\254\231\254\231\254\231\254\231\254\231\254\231\254\231\254\
\000\000\190\000\207\000\197\255\212\255\207\000\207\000\207\000\
\252\254\252\254\000\000\000\000\220\000\220\000\228\255\228\255\
\228\255\228\255\176\255\000\000\231\254\144\000\207\000\191\255\
\144\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\231\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\008\000\000\000\
\000\000\015\000\022\000\230\255\230\255\230\255\230\255\230\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\245\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\151\000\
\151\000\151\000\151\000\151\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\255\000\000\246\255\000\000\000\000\000\000\000\000\000\000\
\088\000\000\000\249\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\255\000\000\252\255\226\255\067\000\095\000\
\066\255\096\255\000\000\000\000\219\255\061\000\117\255\147\255\
\168\255\198\255\000\000\000\000\000\000\000\000\014\255\170\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\000\000\000\134\000\154\000\201\255\000\000\212\000\
\187\255\189\255\000\000\000\000\000\000"

let yytablesize = 496
let yytable = "\091\000\
\021\000\067\000\068\000\069\000\070\000\097\000\098\000\023\000\
\055\000\089\000\092\000\093\000\094\000\055\000\024\000\056\000\
\079\000\080\000\081\000\088\000\056\000\022\000\106\000\107\000\
\110\000\111\000\112\000\113\000\114\000\115\000\116\000\117\000\
\118\000\119\000\120\000\121\000\122\000\038\000\038\000\038\000\
\001\000\013\000\038\000\038\000\038\000\038\000\038\000\075\000\
\076\000\014\000\038\000\038\000\038\000\038\000\038\000\038\000\
\128\000\127\000\015\000\130\000\016\000\038\000\038\000\017\000\
\038\000\018\000\019\000\039\000\039\000\039\000\077\000\078\000\
\039\000\039\000\039\000\020\000\021\000\038\000\038\000\038\000\
\039\000\039\000\039\000\039\000\039\000\039\000\079\000\080\000\
\081\000\022\000\023\000\039\000\039\000\024\000\039\000\038\000\
\039\000\040\000\040\000\040\000\040\000\041\000\040\000\040\000\
\040\000\042\000\043\000\039\000\039\000\039\000\040\000\040\000\
\040\000\040\000\040\000\040\000\044\000\045\000\045\000\045\000\
\045\000\040\000\040\000\045\000\040\000\025\000\026\000\027\000\
\028\000\029\000\030\000\045\000\045\000\045\000\045\000\045\000\
\045\000\040\000\040\000\040\000\046\000\047\000\045\000\045\000\
\048\000\045\000\049\000\071\000\046\000\046\000\046\000\050\000\
\052\000\046\000\034\000\035\000\036\000\037\000\045\000\045\000\
\045\000\046\000\046\000\046\000\046\000\046\000\046\000\053\000\
\054\000\047\000\047\000\047\000\046\000\046\000\047\000\046\000\
\061\000\062\000\007\000\008\000\063\000\064\000\047\000\047\000\
\047\000\047\000\047\000\047\000\046\000\046\000\046\000\072\000\
\055\000\047\000\047\000\073\000\047\000\074\000\124\000\048\000\
\048\000\048\000\090\000\126\000\048\000\057\000\058\000\059\000\
\060\000\047\000\047\000\047\000\048\000\048\000\048\000\048\000\
\048\000\048\000\125\000\129\000\043\000\043\000\043\000\048\000\
\048\000\043\000\048\000\049\000\049\000\049\000\057\000\009\000\
\049\000\043\000\043\000\095\000\096\000\097\000\098\000\048\000\
\048\000\048\000\075\000\084\000\043\000\043\000\010\000\043\000\
\029\000\029\000\053\000\049\000\049\000\054\000\049\000\011\000\
\051\000\000\000\000\000\000\000\043\000\043\000\043\000\000\000\
\000\000\077\000\078\000\049\000\049\000\049\000\000\000\029\000\
\029\000\000\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\079\000\080\000\081\000\000\000\000\000\000\000\029\000\
\029\000\029\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\044\000\044\000\
\044\000\000\000\000\000\044\000\051\000\051\000\051\000\000\000\
\000\000\051\000\000\000\044\000\044\000\004\000\005\000\006\000\
\007\000\008\000\009\000\010\000\000\000\000\000\044\000\044\000\
\000\000\044\000\030\000\030\000\051\000\051\000\000\000\051\000\
\050\000\050\000\050\000\000\000\000\000\050\000\044\000\044\000\
\044\000\000\000\000\000\000\000\051\000\051\000\051\000\075\000\
\085\000\030\000\030\000\000\000\030\000\075\000\086\000\000\000\
\050\000\050\000\000\000\050\000\075\000\087\000\000\000\000\000\
\000\000\030\000\030\000\030\000\000\000\000\000\077\000\078\000\
\050\000\050\000\050\000\000\000\077\000\078\000\000\000\075\000\
\105\000\000\000\075\000\077\000\078\000\000\000\079\000\080\000\
\081\000\027\000\027\000\000\000\079\000\080\000\081\000\000\000\
\000\000\000\000\000\000\079\000\080\000\081\000\077\000\078\000\
\000\000\077\000\078\000\000\000\032\000\032\000\000\000\000\000\
\027\000\027\000\000\000\000\000\000\000\000\000\079\000\080\000\
\081\000\079\000\080\000\081\000\000\000\000\000\000\000\123\000\
\027\000\027\000\027\000\032\000\032\000\095\000\096\000\097\000\
\098\000\000\000\000\000\000\000\099\000\100\000\101\000\102\000\
\103\000\104\000\000\000\032\000\032\000\032\000\095\000\096\000\
\097\000\098\000\000\000\000\000\000\000\099\000\100\000\101\000\
\102\000\103\000\104\000\095\000\096\000\097\000\098\000\000\000\
\000\000\000\000\000\000\000\000\101\000\102\000\103\000\104\000"

let yycheck = "\001\001\
\000\000\057\000\058\000\059\000\060\000\010\001\011\001\000\000\
\002\001\077\000\012\001\013\001\014\001\007\001\000\000\002\001\
\042\001\043\001\044\001\075\000\007\001\000\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\002\001\003\001\004\001\
\001\000\044\001\007\001\008\001\009\001\010\001\011\001\003\001\
\004\001\044\001\015\001\016\001\017\001\018\001\019\001\020\001\
\126\000\125\000\044\001\129\000\044\001\026\001\027\001\044\001\
\029\001\044\001\044\001\002\001\003\001\004\001\026\001\027\001\
\007\001\008\001\009\001\001\001\001\001\042\001\043\001\044\001\
\015\001\016\001\017\001\018\001\019\001\020\001\042\001\043\001\
\044\001\001\001\001\001\026\001\027\001\001\001\029\001\044\001\
\044\001\002\001\003\001\004\001\044\001\044\001\007\001\008\001\
\009\001\044\001\044\001\042\001\043\001\044\001\015\001\016\001\
\017\001\018\001\019\001\020\001\002\001\007\001\002\001\003\001\
\004\001\026\001\027\001\007\001\029\001\035\001\036\001\037\001\
\038\001\039\001\040\001\015\001\016\001\017\001\018\001\019\001\
\020\001\042\001\043\001\044\001\002\001\002\001\026\001\027\001\
\002\001\029\001\002\001\044\001\002\001\003\001\004\001\003\001\
\003\001\007\001\021\000\022\000\023\000\024\000\042\001\043\001\
\044\001\015\001\016\001\017\001\018\001\019\001\020\001\003\001\
\003\001\002\001\003\001\004\001\026\001\027\001\007\001\029\001\
\035\001\036\001\037\001\038\001\039\001\040\001\015\001\016\001\
\017\001\018\001\019\001\020\001\042\001\043\001\044\001\044\001\
\003\001\026\001\027\001\044\001\029\001\044\001\002\001\002\001\
\003\001\004\001\001\001\028\001\007\001\052\000\053\000\054\000\
\055\000\042\001\043\001\044\001\015\001\016\001\017\001\018\001\
\019\001\020\001\007\001\029\001\002\001\003\001\004\001\026\001\
\027\001\007\001\029\001\002\001\003\001\004\001\000\000\002\001\
\007\001\015\001\016\001\008\001\009\001\010\001\011\001\042\001\
\043\001\044\001\003\001\004\001\026\001\027\001\002\001\029\001\
\003\001\004\001\002\001\026\001\027\001\002\001\029\001\003\000\
\045\000\255\255\255\255\255\255\042\001\043\001\044\001\255\255\
\255\255\026\001\027\001\042\001\043\001\044\001\255\255\026\001\
\027\001\255\255\029\001\255\255\255\255\255\255\255\255\255\255\
\255\255\042\001\043\001\044\001\255\255\255\255\255\255\042\001\
\043\001\044\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\034\001\035\001\036\001\037\001\038\001\039\001\040\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\002\001\003\001\
\004\001\255\255\255\255\007\001\002\001\003\001\004\001\255\255\
\255\255\007\001\255\255\015\001\016\001\034\001\035\001\036\001\
\037\001\038\001\039\001\040\001\255\255\255\255\026\001\027\001\
\255\255\029\001\003\001\004\001\026\001\027\001\255\255\029\001\
\002\001\003\001\004\001\255\255\255\255\007\001\042\001\043\001\
\044\001\255\255\255\255\255\255\042\001\043\001\044\001\003\001\
\004\001\026\001\027\001\255\255\029\001\003\001\004\001\255\255\
\026\001\027\001\255\255\029\001\003\001\004\001\255\255\255\255\
\255\255\042\001\043\001\044\001\255\255\255\255\026\001\027\001\
\042\001\043\001\044\001\255\255\026\001\027\001\255\255\003\001\
\004\001\255\255\003\001\026\001\027\001\255\255\042\001\043\001\
\044\001\003\001\004\001\255\255\042\001\043\001\044\001\255\255\
\255\255\255\255\255\255\042\001\043\001\044\001\026\001\027\001\
\255\255\026\001\027\001\255\255\003\001\004\001\255\255\255\255\
\026\001\027\001\255\255\255\255\255\255\255\255\042\001\043\001\
\044\001\042\001\043\001\044\001\255\255\255\255\255\255\002\001\
\042\001\043\001\044\001\026\001\027\001\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\042\001\043\001\044\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001"

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
# 376 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 32 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 384 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 33 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 392 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 37 "parser.mly"
       ( {
          fname = _2;
          formals = _4;
          locals = List.rev _7;
          body = List.rev _8 } )
# 406 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 43 "parser.mly"
      ( {
         return = int_type;
         fname = _2;
         formals = _4;
         locals = List.rev _7;
         body = List.rev _8 } )
# 421 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 50 "parser.mly"
      ( {
         return = str_type;
         fname = _2;
         formals = _4;
         locals = List.rev _7;
         body = List.rev _8 } )
# 436 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 57 "parser.mly"
      ( {
         return = path_type;
         fname = _2;
         formals = _4;
         locals = List.rev _7;
         body = List.rev _8 } )
# 451 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 64 "parser.mly"
      ( {
         return = bool_type;
         fname = _2;
         formals = _4;
         locals = List.rev _7;
         body = List.rev _8 } )
# 466 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                  ( [] )
# 472 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 73 "parser.mly"
                  ( List.rev _1 )
# 479 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 76 "parser.mly"
                             ( [_1] )
# 486 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 77 "parser.mly"
                             ( _3 :: _1 )
# 494 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
             ( { var_type = int_type; name = _2; } )
# 501 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
            ( { var_type = bool_type; name = _2; } )
# 508 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
            ( { var_type = path_type; name = _2; } )
# 515 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
           ( { var_type = str_type; name = _2; } )
# 522 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
             ( { var_type = dict_type; name = _2; } )
# 529 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
             ( { var_type = list_type; name = _2; } )
# 536 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
                     ( [] )
# 542 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 90 "parser.mly"
                     ( _2 :: _1 )
# 550 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
                ({var_type = Ast.Int;  var_name = _2; data_type = Ast.Int})
# 557 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
                ({var_type = Ast.Bool; var_name = _2; data_type = Ast.Bool})
# 564 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parser.mly"
                ({var_type = Ast.Str;  var_name = _2; data_type = Ast.Str})
# 571 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
                ({var_type = Ast.Path; var_name = _2; data_type = Ast.Path})
# 578 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
                ({var_type = Ast.Dict; var_name = _2; data_type = Ast.Dict})
# 585 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
                ({var_type = Ast.List; var_name = _2; data_type = Ast.List})
# 592 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                   ( [] )
# 598 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "parser.mly"
                   ( _2 :: _1 )
# 606 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
         ( Expr(_1) )
# 613 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                                                 ( Return(_2) )
# 620 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 107 "parser.mly"
                                                 ( Block(List.rev _2) )
# 627 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 108 "parser.mly"
                                                 ( If(_3, _6, Block([])) )
# 635 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 109 "parser.mly"
                                                 ( If(_3, _6, _8) )
# 644 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                  ( Noexpr )
# 650 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                  ( _1 )
# 657 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 116 "parser.mly"
                                 ( LitInt(_1) )
# 664 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                                 ( LitStr(_1) )
# 671 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "parser.mly"
                                 ( Id(_1) )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                 ( Binop(_1, Add,      _3) )
# 686 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                                 ( Binop(_1, Sub,      _3) )
# 694 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                                 ( Binop(_1, Mult,     _3) )
# 702 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                                 ( Binop(_1, Div,      _3) )
# 710 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                 ( Binop(_1, Equal,    _3) )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                 ( Binop(_1, Neq,      _3) )
# 726 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                 ( Binop(_1, Less,     _3) )
# 734 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                 ( Binop(_1, Leq,      _3) )
# 742 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                                 ( Binop(_1, Greater,  _3) )
# 750 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                 ( Binop(_1, Geq,      _3) )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                 ( Assign(_1, _3) )
# 766 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                 ( Copy(_1,   _3) )
# 774 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                                 ( Assign(_1, _3) )
# 782 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 132 "parser.mly"
                                 ( Call(_1,   _3) )
# 790 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
                  ( [] )
# 796 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 136 "parser.mly"
                  ( List.rev _1 )
# 803 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                            ( [_1] )
# 810 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                            ( _3 :: _1 )
# 818 "parser.ml"
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
