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
\011\000\011\000\011\000\014\000\014\000\014\000\014\000\013\000\
\013\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\017\000\017\000\
\017\000\015\000\015\000\016\000\016\000\018\000\018\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\000\000\001\000\001\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\000\000\001\000\001\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\000\000\002\000\002\000\003\000\006\000\008\000\003\000\005\000\
\007\000\006\000\008\000\001\000\001\000\001\000\001\000\000\000\
\001\000\001\000\001\000\001\000\003\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\002\000\001\000\001\000\
\001\000\001\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\003\000\005\000\006\000\009\000\
\010\000\011\000\008\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\027\000\029\000\
\031\000\032\000\030\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\016\000\019\000\
\020\000\021\000\018\000\017\000\000\000\000\000\000\000\015\000\
\024\000\033\000\000\000\000\000\025\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\050\000\051\000\052\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\045\000\
\046\000\047\000\044\000\000\000\000\000\000\000\000\000\000\000\
\000\000\071\000\072\000\073\000\070\000\035\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\053\000\036\000\000\000\000\000\
\000\000\000\000\039\000\000\000\000\000\000\000\000\000\000\000\
\000\000\057\000\058\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\075\000\000\000\000\000\000\000\
\000\000\069\000\000\000\000\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\041\000\038\000\043\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\021\000\044\000\058\000\060\000\045\000\
\046\000\059\000\073\000\074\000\078\000\084\000\076\000\117\000\
\093\000\118\000"

let yysindex = "\010\000\
\000\000\000\000\241\000\081\255\219\254\220\254\226\254\232\254\
\251\254\253\254\011\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\255\041\255\058\255\070\255\
\080\255\082\255\084\255\089\255\091\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\220\255\046\255\053\255\054\255\
\056\255\073\255\074\255\137\255\131\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\139\255\220\255\248\000\000\000\
\000\000\000\000\248\000\043\255\000\000\000\000\007\255\007\255\
\001\255\142\255\144\255\007\255\000\000\000\000\000\000\014\255\
\000\000\082\000\063\000\134\255\120\000\138\255\007\255\000\000\
\000\000\000\000\000\000\113\255\120\255\007\255\101\000\007\255\
\007\255\000\000\000\000\000\000\000\000\000\000\007\255\007\255\
\007\255\007\255\007\255\007\255\007\255\007\255\007\255\007\255\
\007\255\007\255\007\255\007\255\000\000\000\000\255\255\120\255\
\119\255\023\000\000\000\120\000\151\255\148\255\120\000\247\254\
\247\254\000\000\000\000\120\000\120\000\135\000\135\000\252\254\
\252\254\252\254\252\254\016\000\000\000\133\255\146\255\120\255\
\136\255\000\000\007\255\136\255\136\255\164\255\000\000\120\000\
\147\255\153\255\136\255\136\255\136\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\167\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\167\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\175\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\052\255\000\000\
\000\000\000\000\083\255\000\000\000\000\000\000\000\000\176\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\189\255\
\000\000\000\000\180\255\000\000\183\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\191\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\066\255\000\000\192\255\076\255\210\255\
\231\255\000\000\000\000\107\255\152\255\240\000\001\001\156\000\
\177\000\198\000\219\000\044\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\067\255\
\097\255\128\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\228\255\000\000\000\000\000\000\000\000\000\000\000\000\
\143\000\000\000\129\255\193\255\000\000\190\255\095\000\000\000\
\000\000\000\000"

let yytablesize = 547
let yytable = "\075\000\
\077\000\079\000\097\000\098\000\087\000\095\000\096\000\097\000\
\098\000\143\000\001\000\063\000\145\000\146\000\088\000\111\000\
\022\000\023\000\113\000\150\000\151\000\152\000\114\000\024\000\
\116\000\119\000\057\000\089\000\107\000\025\000\061\000\120\000\
\121\000\122\000\123\000\124\000\125\000\126\000\127\000\128\000\
\129\000\130\000\131\000\132\000\075\000\135\000\062\000\063\000\
\026\000\030\000\027\000\080\000\081\000\082\000\083\000\022\000\
\022\000\069\000\070\000\071\000\072\000\090\000\091\000\092\000\
\028\000\029\000\031\000\078\000\079\000\142\000\064\000\065\000\
\078\000\079\000\066\000\144\000\067\000\066\000\032\000\022\000\
\022\000\066\000\066\000\022\000\066\000\022\000\023\000\023\000\
\033\000\068\000\034\000\037\000\035\000\069\000\070\000\071\000\
\072\000\036\000\022\000\047\000\037\000\037\000\022\000\022\000\
\022\000\022\000\048\000\049\000\068\000\050\000\023\000\023\000\
\068\000\068\000\023\000\068\000\023\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\037\000\037\000\051\000\052\000\
\037\000\023\000\037\000\042\000\042\000\023\000\023\000\023\000\
\023\000\054\000\053\000\109\000\063\000\055\000\085\000\037\000\
\086\000\112\000\110\000\037\000\037\000\037\000\037\000\136\000\
\138\000\067\000\139\000\042\000\042\000\067\000\067\000\042\000\
\067\000\042\000\140\000\064\000\065\000\147\000\080\000\066\000\
\012\000\067\000\080\000\081\000\082\000\083\000\042\000\141\000\
\013\000\148\000\042\000\042\000\042\000\042\000\068\000\149\000\
\048\000\074\000\069\000\070\000\071\000\072\000\054\000\049\000\
\076\000\077\000\054\000\054\000\056\000\054\000\054\000\054\000\
\054\000\054\000\133\000\054\000\054\000\054\000\054\000\054\000\
\054\000\054\000\054\000\055\000\000\000\000\000\000\000\055\000\
\055\000\000\000\055\000\055\000\055\000\054\000\000\000\000\000\
\055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
\056\000\000\000\000\000\000\000\056\000\056\000\000\000\056\000\
\056\000\056\000\055\000\000\000\000\000\056\000\056\000\056\000\
\056\000\056\000\056\000\056\000\056\000\000\000\000\000\000\000\
\134\000\038\000\039\000\040\000\041\000\042\000\043\000\056\000\
\095\000\096\000\097\000\098\000\000\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\000\000\000\000\000\000\
\137\000\095\000\096\000\097\000\098\000\000\000\000\000\107\000\
\095\000\096\000\097\000\098\000\000\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\059\000\000\000\000\000\
\000\000\059\000\059\000\000\000\059\000\000\000\000\000\107\000\
\000\000\000\000\059\000\059\000\059\000\059\000\059\000\059\000\
\059\000\059\000\000\000\000\000\000\000\108\000\000\000\000\000\
\095\000\096\000\097\000\098\000\059\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\000\000\000\000\000\000\
\000\000\000\000\094\000\095\000\096\000\097\000\098\000\107\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\000\000\000\000\000\000\000\000\000\000\115\000\095\000\096\000\
\097\000\098\000\107\000\099\000\100\000\101\000\102\000\103\000\
\104\000\105\000\106\000\000\000\000\000\000\000\000\000\000\000\
\000\000\095\000\096\000\097\000\098\000\107\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\000\000\000\000\
\095\000\096\000\097\000\098\000\000\000\000\000\000\000\000\000\
\107\000\103\000\104\000\105\000\106\000\062\000\000\000\000\000\
\000\000\062\000\062\000\000\000\062\000\000\000\000\000\107\000\
\000\000\000\000\062\000\062\000\062\000\062\000\062\000\062\000\
\062\000\062\000\063\000\000\000\000\000\000\000\063\000\063\000\
\000\000\063\000\000\000\000\000\000\000\000\000\000\000\063\000\
\063\000\063\000\063\000\063\000\063\000\063\000\063\000\064\000\
\000\000\000\000\000\000\064\000\064\000\000\000\064\000\000\000\
\000\000\000\000\000\000\000\000\064\000\064\000\064\000\064\000\
\064\000\064\000\064\000\064\000\065\000\000\000\000\000\000\000\
\065\000\065\000\000\000\065\000\000\000\000\000\000\000\000\000\
\000\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
\065\000\060\000\000\000\000\000\000\000\060\000\060\000\000\000\
\060\000\000\000\000\000\000\000\000\000\000\000\060\000\060\000\
\060\000\060\000\061\000\000\000\000\000\000\000\061\000\061\000\
\000\000\061\000\000\000\000\000\000\000\000\000\000\000\061\000\
\061\000\061\000\061\000\000\000\004\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000"

let yycheck = "\063\000\
\064\000\001\001\012\001\013\001\068\000\010\001\011\001\012\001\
\013\001\137\000\001\000\005\001\140\000\141\000\001\001\079\000\
\054\001\054\001\085\000\147\000\148\000\149\000\086\000\054\001\
\088\000\089\000\055\000\014\001\033\001\054\001\059\000\095\000\
\096\000\097\000\098\000\099\000\100\000\101\000\102\000\103\000\
\104\000\105\000\106\000\107\000\108\000\112\000\004\001\005\001\
\054\001\009\001\054\001\051\001\052\001\053\001\054\001\004\001\
\005\001\051\001\052\001\053\001\054\001\048\001\049\001\050\001\
\054\001\054\001\009\001\002\001\002\001\136\000\028\001\029\001\
\007\001\007\001\032\001\139\000\034\001\002\001\009\001\028\001\
\029\001\006\001\007\001\032\001\009\001\034\001\004\001\005\001\
\009\001\047\001\009\001\001\001\009\001\051\001\052\001\053\001\
\054\001\009\001\047\001\054\001\004\001\005\001\051\001\052\001\
\053\001\054\001\054\001\054\001\002\001\054\001\028\001\029\001\
\006\001\007\001\032\001\009\001\034\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\028\001\029\001\054\001\054\001\
\032\001\047\001\034\001\004\001\005\001\051\001\052\001\053\001\
\054\001\007\001\002\001\006\001\005\001\003\001\001\001\047\001\
\001\001\033\001\009\001\051\001\052\001\053\001\054\001\033\001\
\002\001\002\001\007\001\028\001\029\001\006\001\007\001\032\001\
\009\001\034\001\030\001\028\001\029\001\002\001\000\000\032\001\
\002\001\034\001\051\001\052\001\053\001\054\001\047\001\030\001\
\002\001\031\001\051\001\052\001\053\001\054\001\047\001\031\001\
\009\001\006\001\051\001\052\001\053\001\054\001\002\001\009\001\
\002\001\002\001\006\001\007\001\054\000\009\001\010\001\011\001\
\012\001\013\001\108\000\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\002\001\255\255\255\255\255\255\006\001\
\007\001\255\255\009\001\010\001\011\001\033\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\002\001\255\255\255\255\255\255\006\001\007\001\255\255\009\001\
\010\001\011\001\033\001\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\002\001\038\001\039\001\040\001\041\001\042\001\043\001\033\001\
\010\001\011\001\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\002\001\010\001\011\001\012\001\013\001\255\255\255\255\033\001\
\010\001\011\001\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\002\001\255\255\255\255\
\255\255\006\001\007\001\255\255\009\001\255\255\255\255\033\001\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\255\255\255\255\007\001\255\255\255\255\
\010\001\011\001\012\001\013\001\033\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\255\255\255\255\009\001\010\001\011\001\012\001\013\001\033\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\255\255\255\255\255\255\255\255\009\001\010\001\011\001\
\012\001\013\001\033\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\255\255\255\255\255\255\255\255\
\255\255\010\001\011\001\012\001\013\001\033\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\255\255\
\010\001\011\001\012\001\013\001\255\255\255\255\255\255\255\255\
\033\001\019\001\020\001\021\001\022\001\002\001\255\255\255\255\
\255\255\006\001\007\001\255\255\009\001\255\255\255\255\033\001\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\002\001\255\255\255\255\255\255\006\001\007\001\
\255\255\009\001\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\002\001\
\255\255\255\255\255\255\006\001\007\001\255\255\009\001\255\255\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\002\001\255\255\255\255\255\255\
\006\001\007\001\255\255\009\001\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\002\001\255\255\255\255\255\255\006\001\007\001\255\255\
\009\001\255\255\255\255\255\255\255\255\255\255\015\001\016\001\
\017\001\018\001\002\001\255\255\255\255\255\255\006\001\007\001\
\255\255\009\001\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\255\255\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\037\001\038\001\039\001\040\001\
\041\001\042\001\043\001"

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
# 432 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 440 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 448 "parser.ml"
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
# 464 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                ( VoidType )
# 470 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( IntType )
# 476 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                ( BoolType )
# 482 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                ( PathType )
# 488 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                ( StrType )
# 494 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                ( DictType )
# 500 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                ( ListType )
# 506 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
    ( [] )
# 512 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 59 "parser.mly"
                    ( List.rev _1 )
# 519 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 62 "parser.mly"
                               ( [_1] )
# 526 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 63 "parser.mly"
                               ( _3 :: _1 )
# 534 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                  ( { vtype = IntType;  vname = _2; } )
# 541 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                  ( { vtype = BoolType; vname = _2; } )
# 548 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                  ( { vtype = PathType; vname = _2; } )
# 555 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                  ( { vtype = StrType;  vname = _2; } )
# 562 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
                  ( { vtype = DictType; vname = _2; } )
# 569 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                  ( { vtype = ListType; vname = _2; } )
# 576 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
    ( [] )
# 582 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 76 "parser.mly"
                    ( List.rev _1 )
# 589 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 79 "parser.mly"
                       ( [_1] )
# 596 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 80 "parser.mly"
                       ( _2 :: _1 )
# 604 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 85 "parser.mly"
                    ( { vtype = VoidType;  vname = _2; } )
# 611 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
                     ( { vtype = IntType;  vname = _2; } )
# 618 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parser.mly"
                   ( { vtype = BoolType; vname = _2; } )
# 625 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 88 "parser.mly"
                   ( { vtype = StrType;  vname = _2; } )
# 632 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 89 "parser.mly"
                   ( { vtype = PathType; vname = _2; } )
# 639 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 90 "parser.mly"
                   ( { vtype = DictType; vname = _2; } )
# 646 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 91 "parser.mly"
                   ( { vtype = ListType; vname = _2; } )
# 653 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
    ( [] )
# 659 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "parser.mly"
                     ( _2 :: _1 )
# 667 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                                                   ( Expr(_1) )
# 674 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 100 "parser.mly"
                                                   ( Return(_2) )
# 681 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 689 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 698 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                                                   ( Print(_2) )
# 705 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                                ( While(_3, Block([_5])) )
# 713 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'list_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'list_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                                                           ( For(_3, _5, _7) )
# 722 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'list_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'list_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                                                        ( Ifin(_2, _4, _6, Block([])) )
# 731 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'list_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'list_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                                        ( Ifin(_2, _4, _6, _8) )
# 741 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
                                  ( ListId(_1) )
# 748 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "parser.mly"
                                   ( ListItemInt(_1) )
# 755 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "parser.mly"
                                   ( ListItemStr(_1) )
# 762 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 113 "parser.mly"
                                   ( ListItemBool(_1) )
# 769 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                  ( Noexpr )
# 775 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                  ( _1 )
# 782 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 120 "parser.mly"
                                   ( LitInt(_1) )
# 789 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
                                   ( LitStr(_1) )
# 796 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 122 "parser.mly"
                            ( LitBool(_1) )
# 803 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_items) in
    Obj.repr(
# 123 "parser.mly"
                                   ( List(_2) )
# 810 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "parser.mly"
                                   ( Id(_1) )
# 817 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 825 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 841 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 849 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                   ( Binop(_1, In,       _3) )
# 857 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 865 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 873 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 881 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 889 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 897 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 905 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                                   ( Assign(_1, _3) )
# 913 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                                     ( Copy(_1,   _3) )
# 921 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                                     ( Move(_1,  _3) )
# 929 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 139 "parser.mly"
                                   ( Call(_1,   _3) )
# 937 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathattributes) in
    Obj.repr(
# 140 "parser.mly"
                                   ( Pathattr(_1, _2) )
# 945 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
                                   ( Pathname )
# 951 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
                                   ( Pathcreated )
# 957 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "parser.mly"
                                   ( Pathkind )
# 963 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                                   ( Item(_1) )
# 970 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_items) in
    Obj.repr(
# 149 "parser.mly"
                                   ( Seq(_1, Comma, _3) )
# 978 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
                    ( [] )
# 984 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 153 "parser.mly"
                    ( List.rev _1 )
# 991 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
                              ( [_1] )
# 998 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
                              ( _3 :: _1 )
# 1006 "parser.ml"
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
