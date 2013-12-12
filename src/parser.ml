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
  | PATHNAME
  | PATHCREATED
  | PATHKIND
  | ADD
  | REMOVE
  | LIT_INT of (int)
  | LIT_STR of (string)
  | LIT_BOOL of (bool)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 65 "parser.ml"
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
  304 (* PATHNAME *);
  305 (* PATHCREATED *);
  306 (* PATHKIND *);
  307 (* ADD *);
  308 (* REMOVE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  309 (* LIT_INT *);
  310 (* LIT_STR *);
  311 (* LIT_BOOL *);
  312 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\008\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\006\000\006\000\010\000\
\010\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\007\000\007\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\015\000\016\000\016\000\
\016\000\016\000\014\000\014\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\019\000\019\000\019\000\017\000\017\000\
\018\000\018\000\020\000\020\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\000\000\001\000\001\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\000\000\001\000\001\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\000\000\002\000\001\000\002\000\003\000\006\000\008\000\003\000\
\005\000\007\000\006\000\008\000\003\000\001\000\001\000\001\000\
\001\000\001\000\000\000\001\000\001\000\001\000\001\000\003\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\002\000\005\000\005\000\001\000\001\000\001\000\001\000\003\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\003\000\005\000\006\000\009\000\
\010\000\011\000\008\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\027\000\029\000\
\031\000\032\000\030\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\016\000\019\000\
\020\000\021\000\018\000\017\000\000\000\000\000\000\000\015\000\
\024\000\033\000\000\000\000\000\025\000\033\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\000\054\000\055\000\
\000\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\048\000\049\000\050\000\047\000\000\000\000\000\
\000\000\000\000\000\000\000\000\076\000\077\000\078\000\000\000\
\000\000\073\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\045\000\000\000\056\000\037\000\000\000\000\000\046\000\000\000\
\000\000\040\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\060\000\061\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\080\000\000\000\000\000\
\000\000\000\000\072\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\000\000\074\000\075\000\000\000\000\000\000\000\
\000\000\000\000\042\000\039\000\044\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\021\000\044\000\058\000\060\000\045\000\
\046\000\059\000\074\000\077\000\075\000\081\000\120\000\087\000\
\079\000\124\000\098\000\125\000"

let yysindex = "\003\000\
\000\000\000\000\240\000\247\000\211\254\224\254\226\254\234\254\
\249\254\002\255\003\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\255\060\255\066\255\074\255\
\076\255\079\255\080\255\082\255\092\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\122\255\038\255\043\255\044\255\
\053\255\054\255\064\255\126\255\123\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\130\255\122\255\255\000\000\000\
\000\000\000\000\255\000\048\255\000\000\000\000\000\000\017\255\
\017\255\001\255\128\255\133\255\017\255\000\000\000\000\000\000\
\016\255\000\000\096\000\148\255\132\255\077\000\139\255\134\000\
\129\255\017\255\000\000\000\000\000\000\000\000\106\255\088\255\
\017\255\115\000\017\255\017\255\000\000\000\000\000\000\151\255\
\153\255\000\000\000\000\017\255\017\255\017\255\017\255\017\255\
\017\255\017\255\017\255\017\255\017\255\017\255\017\255\017\255\
\000\000\017\255\000\000\000\000\013\000\062\255\000\000\135\255\
\037\000\000\000\134\000\167\255\164\255\134\000\062\255\062\255\
\020\255\020\255\000\000\000\000\134\000\134\000\149\000\149\000\
\252\254\252\254\252\254\252\254\186\255\000\000\125\255\143\255\
\088\255\148\255\000\000\017\255\173\255\181\255\148\255\148\255\
\182\255\000\000\134\000\000\000\000\000\155\255\169\255\148\255\
\148\255\148\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\206\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\205\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\206\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\058\255\000\000\
\000\000\000\000\093\255\000\000\000\000\000\000\000\000\000\000\
\202\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\203\255\000\000\000\000\213\255\000\000\221\255\000\000\219\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\227\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\255\000\000\230\255\014\255\000\000\000\000\
\224\255\245\255\000\000\000\000\072\255\117\255\172\255\254\000\
\170\000\191\000\212\000\233\000\058\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\255\000\000\000\000\103\255\138\255\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\232\255\000\000\000\000\000\000\000\000\175\000\000\000\
\184\000\000\000\113\001\000\000\192\255\000\000\103\000\141\255\
\135\000\000\000\000\000\000\000"

let yytablesize = 554
let yytable = "\078\000\
\080\000\082\000\144\000\001\000\090\000\100\000\101\000\102\000\
\103\000\083\000\022\000\149\000\150\000\084\000\083\000\069\000\
\091\000\117\000\084\000\069\000\069\000\064\000\069\000\023\000\
\121\000\024\000\123\000\126\000\112\000\092\000\057\000\102\000\
\103\000\025\000\061\000\129\000\130\000\131\000\132\000\133\000\
\134\000\135\000\136\000\137\000\138\000\139\000\140\000\141\000\
\026\000\078\000\062\000\063\000\064\000\083\000\084\000\085\000\
\086\000\027\000\028\000\029\000\022\000\022\000\022\000\093\000\
\094\000\095\000\096\000\097\000\030\000\070\000\071\000\072\000\
\073\000\071\000\031\000\065\000\066\000\071\000\071\000\067\000\
\071\000\068\000\032\000\155\000\033\000\022\000\022\000\034\000\
\035\000\022\000\036\000\022\000\037\000\047\000\069\000\023\000\
\023\000\023\000\048\000\049\000\070\000\071\000\072\000\073\000\
\022\000\038\000\038\000\038\000\050\000\051\000\022\000\022\000\
\022\000\022\000\083\000\084\000\085\000\086\000\070\000\052\000\
\023\000\023\000\070\000\070\000\023\000\070\000\023\000\053\000\
\088\000\054\000\038\000\038\000\055\000\089\000\038\000\113\000\
\038\000\116\000\118\000\023\000\043\000\043\000\043\000\119\000\
\115\000\023\000\023\000\023\000\023\000\038\000\062\000\127\000\
\064\000\128\000\151\000\038\000\038\000\038\000\038\000\038\000\
\039\000\040\000\041\000\042\000\043\000\043\000\043\000\145\000\
\147\000\043\000\148\000\043\000\152\000\063\000\156\000\065\000\
\066\000\063\000\063\000\067\000\063\000\068\000\157\000\160\000\
\043\000\161\000\063\000\063\000\063\000\063\000\043\000\043\000\
\043\000\043\000\069\000\100\000\101\000\102\000\103\000\162\000\
\070\000\071\000\072\000\073\000\057\000\085\000\012\000\013\000\
\057\000\057\000\051\000\057\000\057\000\057\000\057\000\057\000\
\035\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\058\000\079\000\052\000\081\000\058\000\058\000\082\000\
\058\000\058\000\058\000\057\000\076\000\056\000\058\000\058\000\
\058\000\058\000\058\000\058\000\058\000\058\000\059\000\153\000\
\142\000\000\000\059\000\059\000\000\000\059\000\059\000\059\000\
\058\000\000\000\000\000\059\000\059\000\059\000\059\000\059\000\
\059\000\059\000\059\000\000\000\000\000\000\000\143\000\000\000\
\000\000\000\000\000\000\000\000\000\000\059\000\100\000\101\000\
\102\000\103\000\000\000\104\000\105\000\106\000\107\000\108\000\
\109\000\110\000\111\000\000\000\000\000\000\000\146\000\000\000\
\000\000\000\000\000\000\000\000\000\000\112\000\100\000\101\000\
\102\000\103\000\000\000\104\000\105\000\106\000\107\000\108\000\
\109\000\110\000\111\000\062\000\000\000\000\000\000\000\062\000\
\062\000\000\000\062\000\000\000\000\000\112\000\000\000\000\000\
\062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
\000\000\000\000\000\000\114\000\000\000\000\000\100\000\101\000\
\102\000\103\000\062\000\104\000\105\000\106\000\107\000\108\000\
\109\000\110\000\111\000\000\000\000\000\000\000\000\000\000\000\
\099\000\100\000\101\000\102\000\103\000\112\000\104\000\105\000\
\106\000\107\000\108\000\109\000\110\000\111\000\000\000\000\000\
\000\000\000\000\000\000\122\000\100\000\101\000\102\000\103\000\
\112\000\104\000\105\000\106\000\107\000\108\000\109\000\110\000\
\111\000\000\000\000\000\000\000\000\000\000\000\000\000\100\000\
\101\000\102\000\103\000\112\000\104\000\105\000\106\000\107\000\
\108\000\109\000\110\000\111\000\000\000\000\000\100\000\101\000\
\102\000\103\000\000\000\000\000\000\000\000\000\112\000\108\000\
\109\000\110\000\111\000\065\000\000\000\000\000\000\000\065\000\
\065\000\000\000\065\000\000\000\000\000\112\000\000\000\000\000\
\065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
\066\000\000\000\000\000\000\000\066\000\066\000\000\000\066\000\
\000\000\000\000\000\000\000\000\000\000\066\000\066\000\066\000\
\066\000\066\000\066\000\066\000\066\000\067\000\000\000\000\000\
\000\000\067\000\067\000\000\000\067\000\000\000\000\000\000\000\
\000\000\000\000\067\000\067\000\067\000\067\000\067\000\067\000\
\067\000\067\000\068\000\000\000\000\000\000\000\068\000\068\000\
\000\000\068\000\000\000\000\000\000\000\000\000\000\000\068\000\
\068\000\068\000\068\000\068\000\068\000\068\000\068\000\064\000\
\000\000\000\000\154\000\064\000\064\000\000\000\064\000\158\000\
\159\000\000\000\000\000\000\000\064\000\064\000\064\000\064\000\
\163\000\164\000\165\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\014\000\015\000\016\000\017\000\018\000\
\019\000\020\000\000\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000"

let yycheck = "\064\000\
\065\000\001\001\118\000\001\000\069\000\010\001\011\001\012\001\
\013\001\002\001\056\001\127\000\128\000\002\001\007\001\002\001\
\001\001\082\000\007\001\006\001\007\001\005\001\009\001\056\001\
\089\000\056\001\091\000\092\000\033\001\014\001\055\000\012\001\
\013\001\056\001\059\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\107\000\108\000\109\000\110\000\111\000\112\000\
\056\001\114\000\003\001\004\001\005\001\053\001\054\001\055\001\
\056\001\056\001\056\001\056\001\003\001\004\001\005\001\048\001\
\049\001\050\001\051\001\052\001\009\001\053\001\054\001\055\001\
\056\001\002\001\009\001\028\001\029\001\006\001\007\001\032\001\
\009\001\034\001\009\001\148\000\009\001\028\001\029\001\009\001\
\009\001\032\001\009\001\034\001\001\001\056\001\047\001\003\001\
\004\001\005\001\056\001\056\001\053\001\054\001\055\001\056\001\
\047\001\003\001\004\001\005\001\056\001\056\001\053\001\054\001\
\055\001\056\001\053\001\054\001\055\001\056\001\002\001\056\001\
\028\001\029\001\006\001\007\001\032\001\009\001\034\001\002\001\
\001\001\007\001\028\001\029\001\003\001\001\001\032\001\004\001\
\034\001\009\001\033\001\047\001\003\001\004\001\005\001\056\001\
\006\001\053\001\054\001\055\001\056\001\047\001\003\001\001\001\
\005\001\001\001\030\001\053\001\054\001\055\001\056\001\038\001\
\039\001\040\001\041\001\042\001\043\001\028\001\029\001\033\001\
\002\001\032\001\007\001\034\001\030\001\002\001\002\001\028\001\
\029\001\006\001\007\001\032\001\009\001\034\001\002\001\002\001\
\047\001\031\001\015\001\016\001\017\001\018\001\053\001\054\001\
\055\001\056\001\047\001\010\001\011\001\012\001\013\001\031\001\
\053\001\054\001\055\001\056\001\002\001\000\000\002\001\002\001\
\006\001\007\001\009\001\009\001\010\001\011\001\012\001\013\001\
\004\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\002\001\006\001\009\001\002\001\006\001\007\001\002\001\
\009\001\010\001\011\001\033\001\062\000\054\000\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\002\001\145\000\
\114\000\255\255\006\001\007\001\255\255\009\001\010\001\011\001\
\033\001\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\255\255\255\255\002\001\255\255\
\255\255\255\255\255\255\255\255\255\255\033\001\010\001\011\001\
\012\001\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\255\255\255\255\002\001\255\255\
\255\255\255\255\255\255\255\255\255\255\033\001\010\001\011\001\
\012\001\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\002\001\255\255\255\255\255\255\006\001\
\007\001\255\255\009\001\255\255\255\255\033\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\255\255\255\255\007\001\255\255\255\255\010\001\011\001\
\012\001\013\001\033\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\255\255\255\255\255\255\255\255\
\009\001\010\001\011\001\012\001\013\001\033\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\255\255\
\255\255\255\255\255\255\009\001\010\001\011\001\012\001\013\001\
\033\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\255\255\255\255\255\255\010\001\
\011\001\012\001\013\001\033\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\255\255\010\001\011\001\
\012\001\013\001\255\255\255\255\255\255\255\255\033\001\019\001\
\020\001\021\001\022\001\002\001\255\255\255\255\255\255\006\001\
\007\001\255\255\009\001\255\255\255\255\033\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\002\001\255\255\255\255\255\255\006\001\007\001\255\255\009\001\
\255\255\255\255\255\255\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\002\001\255\255\255\255\
\255\255\006\001\007\001\255\255\009\001\255\255\255\255\255\255\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\002\001\255\255\255\255\255\255\006\001\007\001\
\255\255\009\001\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\002\001\
\255\255\255\255\146\000\006\001\007\001\255\255\009\001\151\000\
\152\000\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\160\000\161\000\162\000\036\001\037\001\038\001\039\001\040\001\
\041\001\042\001\043\001\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001\255\255\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001"

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
  PATHNAME\000\
  PATHCREATED\000\
  PATHKIND\000\
  ADD\000\
  REMOVE\000\
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
# 35 "parser.mly"
    ( [], [] )
# 448 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 456 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 464 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'return_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "parser.mly"
       ({
        return = _2;
        fname = _3;
        formals = _5;
        fnlocals = List.rev _8;
        body = List.rev _9 })
# 480 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                ( VoidType )
# 486 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( IntType )
# 492 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                ( BoolType )
# 498 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                ( PathType )
# 504 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                ( StrType )
# 510 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                ( DictType )
# 516 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                ( ListType )
# 522 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
    ( [] )
# 528 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 59 "parser.mly"
                    ( List.rev _1 )
# 535 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 62 "parser.mly"
                               ( [_1] )
# 542 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 63 "parser.mly"
                               ( _3 :: _1 )
# 550 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                  ( { vtype = IntType;  vname = _2; } )
# 557 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                  ( { vtype = BoolType; vname = _2; } )
# 564 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                  ( { vtype = PathType; vname = _2; } )
# 571 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                  ( { vtype = StrType;  vname = _2; } )
# 578 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
                  ( { vtype = DictType; vname = _2; } )
# 585 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                  ( { vtype = ListType; vname = _2; } )
# 592 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
    ( [] )
# 598 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 76 "parser.mly"
                    ( List.rev _1 )
# 605 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 79 "parser.mly"
                       ( [_1] )
# 612 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 80 "parser.mly"
                       ( _2 :: _1 )
# 620 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 85 "parser.mly"
                    ( { vtype = VoidType;  vname = _2; } )
# 627 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
                     ( { vtype = IntType;  vname = _2; } )
# 634 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parser.mly"
                   ( { vtype = BoolType; vname = _2; } )
# 641 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 88 "parser.mly"
                   ( { vtype = StrType;  vname = _2; } )
# 648 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 89 "parser.mly"
                   ( { vtype = PathType; vname = _2; } )
# 655 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 90 "parser.mly"
                   ( { vtype = DictType; vname = _2; } )
# 662 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 91 "parser.mly"
                   ( { vtype = ListType; vname = _2; } )
# 669 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
    ( [] )
# 675 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "parser.mly"
                     ( _2 :: _1 )
# 683 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 98 "parser.mly"
                       ( List.rev _1 )
# 690 "parser.ml"
               : 'rev_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                                                   ( Expr(_1) )
# 697 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 103 "parser.mly"
                                                   ( Return(_2) )
# 704 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 712 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 721 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                                                   ( Print(_2) )
# 728 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                                ( While(_3, _5) )
# 736 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'for_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'for_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 108 "parser.mly"
                                                         ( For(_3, _5, _7 ) )
# 745 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'list_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'list_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 109 "parser.mly"
                                                        ( Ifin(_2, _4, _6, Block([])) )
# 754 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'list_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'list_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 110 "parser.mly"
                                                        ( Ifin(_2, _4, _6, _8) )
# 764 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rev_stmt_list) in
    Obj.repr(
# 111 "parser.mly"
                                                        ( Block(_2) )
# 771 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parser.mly"
                                    ( Forid(_1) )
# 778 "parser.ml"
               : 'for_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                                  ( ListId(_1) )
# 785 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 118 "parser.mly"
                                   ( ListItemInt(_1) )
# 792 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "parser.mly"
                                   ( ListItemStr(_1) )
# 799 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 120 "parser.mly"
                                   ( ListItemBool(_1) )
# 806 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
                  ( Noexpr )
# 812 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                  ( _1 )
# 819 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 127 "parser.mly"
                                   ( LitInt(_1) )
# 826 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 128 "parser.mly"
                                   ( LitStr(_1) )
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 129 "parser.mly"
                            ( LitBool(_1) )
# 840 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_items) in
    Obj.repr(
# 130 "parser.mly"
                                   ( List(_2) )
# 847 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parser.mly"
                                   ( Id(_1) )
# 854 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 862 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 870 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 878 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 886 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                                   ( Binop(_1, In,       _3) )
# 894 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 902 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 910 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 918 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 926 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 934 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 942 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                                   ( Assign(_1, _3) )
# 950 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                                     ( Copy(_1,   _3) )
# 958 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                                     ( Move(_1,  _3) )
# 966 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 146 "parser.mly"
                                   ( Call(_1,   _3) )
# 974 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathattributes) in
    Obj.repr(
# 147 "parser.mly"
                                   ( Pathattr(_1, _2) )
# 982 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_expr) in
    Obj.repr(
# 148 "parser.mly"
                                     ( ListAppend(_1, _4) )
# 990 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_expr) in
    Obj.repr(
# 149 "parser.mly"
                                        ( ListRemove(_1, _4) )
# 998 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
                                   ( Pathname )
# 1004 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "parser.mly"
                                   ( Pathcreated )
# 1010 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 154 "parser.mly"
                                   ( Pathkind )
# 1016 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
                                   ( Item(_1) )
# 1023 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_items) in
    Obj.repr(
# 158 "parser.mly"
                                   ( Seq(_1, Comma, _3) )
# 1031 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "parser.mly"
                    ( [] )
# 1037 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 162 "parser.mly"
                    ( List.rev _1 )
# 1044 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                              ( [_1] )
# 1051 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
                              ( _3 :: _1 )
# 1059 "parser.ml"
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
