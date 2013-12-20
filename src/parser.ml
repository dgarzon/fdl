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
  | PATHEXT
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
  282 (* RETURN *);
  283 (* IF *);
  284 (* THEN *);
  285 (* ELSE *);
  286 (* FOR *);
  287 (* IN *);
  288 (* WHILE *);
  289 (* DO *);
  290 (* DEF *);
  291 (* VOID *);
  292 (* INT *);
  293 (* STR *);
  294 (* LIST *);
  295 (* PATH *);
  296 (* BOOL *);
  297 (* TRASH *);
  298 (* TRUE *);
  299 (* FALSE *);
  300 (* PRINT *);
  301 (* PATHNAME *);
  302 (* PATHCREATED *);
  303 (* PATHKIND *);
  304 (* PATHEXT *);
  305 (* ADD *);
  306 (* REMOVE *);
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
\004\000\004\000\005\000\005\000\008\000\008\000\009\000\009\000\
\009\000\009\000\009\000\006\000\006\000\010\000\010\000\002\000\
\002\000\011\000\011\000\011\000\011\000\011\000\011\000\007\000\
\007\000\014\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\016\000\017\000\017\000\017\000\
\015\000\015\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\020\000\020\000\020\000\020\000\018\000\
\018\000\018\000\019\000\019\000\021\000\021\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\001\000\001\000\001\000\001\000\
\001\000\001\000\000\000\001\000\001\000\003\000\002\000\002\000\
\002\000\002\000\002\000\000\000\001\000\001\000\002\000\003\000\
\005\000\001\000\001\000\001\000\001\000\001\000\001\000\000\000\
\002\000\001\000\002\000\003\000\006\000\008\000\003\000\005\000\
\007\000\006\000\008\000\003\000\001\000\001\000\001\000\001\000\
\000\000\001\000\001\000\001\000\001\000\001\000\003\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\002\000\005\000\005\000\001\000\001\000\001\000\001\000\000\000\
\001\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\026\000\027\000\029\000\031\000\
\030\000\028\000\002\000\003\000\000\000\005\000\006\000\009\000\
\010\000\008\000\007\000\000\000\000\000\000\000\024\000\000\000\
\000\000\000\000\052\000\053\000\051\000\054\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\000\000\076\000\077\000\078\000\079\000\
\000\000\000\000\073\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\018\000\019\000\017\000\016\000\000\000\
\000\000\000\000\055\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\059\000\060\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\082\000\072\000\000\000\047\000\048\000\046\000\000\000\
\000\000\022\000\032\000\000\000\000\000\074\000\075\000\000\000\
\023\000\032\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\044\000\036\000\000\000\000\000\
\045\000\000\000\000\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\000\000\000\000\
\000\000\041\000\038\000\043\000"

let yydgoto = "\002\000\
\003\000\011\000\012\000\020\000\038\000\107\000\112\000\039\000\
\040\000\108\000\013\000\121\000\122\000\124\000\126\000\138\000\
\104\000\042\000\077\000\051\000\078\000"

let yysindex = "\007\000\
\000\000\000\000\029\001\136\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\212\254\000\000\000\000\000\000\
\000\000\000\000\000\000\228\254\074\255\026\255\000\000\039\255\
\166\255\039\255\000\000\000\000\000\000\000\000\010\255\243\000\
\230\254\247\254\251\254\255\254\009\255\062\255\070\255\000\000\
\226\000\092\255\039\255\039\255\000\000\000\000\000\000\000\000\
\099\255\110\255\000\000\000\000\039\255\039\255\039\255\039\255\
\039\255\039\255\039\255\039\255\039\255\039\255\039\255\039\255\
\039\255\039\255\000\000\000\000\000\000\000\000\000\000\109\255\
\166\255\039\255\000\000\037\001\126\255\123\255\037\001\112\255\
\112\255\004\255\004\255\000\000\000\000\037\001\037\001\249\254\
\249\254\200\255\200\255\200\255\200\255\103\255\103\255\145\255\
\000\000\000\000\000\000\039\255\000\000\000\000\000\000\131\255\
\132\255\000\000\000\000\145\255\037\001\000\000\000\000\043\255\
\000\000\000\000\000\000\039\255\000\255\140\255\141\255\039\255\
\004\001\000\000\195\255\142\255\037\001\127\255\039\255\122\255\
\100\255\039\255\021\001\000\000\000\000\000\000\053\000\112\255\
\000\000\124\255\074\000\000\000\130\255\160\255\100\255\195\255\
\195\255\195\255\158\255\000\000\161\255\164\255\195\255\195\255\
\195\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\194\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\194\255\193\255\000\000\000\000\000\000\000\000\246\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\199\255\000\000\
\208\255\000\000\213\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\193\255\000\000\082\255\000\000\216\255\016\255\000\000\
\000\000\011\000\032\000\000\000\000\000\059\255\065\255\226\255\
\179\000\095\000\116\000\137\000\158\000\199\000\210\000\075\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\105\255\097\255\000\000\000\000\000\000\
\000\000\000\000\000\000\211\255\000\000\000\000\000\000\000\000\
\000\000\000\000\219\255\000\000\215\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\135\255\165\255\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\169\255\000\000\000\000\000\000\000\000\112\000\000\000\
\156\000\000\000\000\000\232\255\255\255\000\000\000\000\087\000\
\182\255\157\000\000\000\000\000\000\000"

let yytablesize = 581
let yytable = "\032\000\
\127\000\041\000\053\000\054\000\055\000\056\000\105\000\001\000\
\106\000\021\000\043\000\061\000\062\000\063\000\064\000\055\000\
\056\000\069\000\076\000\079\000\113\000\069\000\069\000\044\000\
\069\000\022\000\025\000\067\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\128\000\026\000\068\000\114\000\115\000\026\000\
\069\000\041\000\101\000\102\000\070\000\103\000\045\000\046\000\
\047\000\048\000\049\000\050\000\071\000\142\000\071\000\072\000\
\071\000\071\000\070\000\071\000\116\000\117\000\070\000\070\000\
\118\000\070\000\119\000\109\000\073\000\020\000\020\000\020\000\
\027\000\028\000\023\000\085\000\027\000\028\000\120\000\024\000\
\085\000\029\000\030\000\125\000\031\000\029\000\030\000\131\000\
\031\000\075\000\086\000\080\000\020\000\020\000\135\000\086\000\
\020\000\139\000\020\000\021\000\021\000\021\000\081\000\096\000\
\053\000\054\000\055\000\056\000\020\000\020\000\020\000\059\000\
\060\000\061\000\062\000\063\000\064\000\020\000\020\000\099\000\
\020\000\100\000\021\000\021\000\110\000\111\000\021\000\134\000\
\021\000\037\000\037\000\037\000\129\000\130\000\148\000\149\000\
\150\000\133\000\021\000\021\000\021\000\154\000\155\000\156\000\
\136\000\137\000\143\000\021\000\021\000\145\000\021\000\151\000\
\037\000\037\000\101\000\102\000\037\000\103\000\037\000\042\000\
\042\000\042\000\014\000\015\000\016\000\017\000\018\000\019\000\
\037\000\037\000\037\000\005\000\006\000\007\000\008\000\009\000\
\010\000\037\000\037\000\146\000\037\000\152\000\042\000\042\000\
\153\000\087\000\042\000\011\000\042\000\114\000\080\000\026\000\
\012\000\033\000\034\000\035\000\036\000\037\000\042\000\042\000\
\042\000\053\000\054\000\055\000\056\000\081\000\083\000\042\000\
\042\000\084\000\042\000\049\000\116\000\117\000\034\000\050\000\
\118\000\123\000\119\000\061\000\097\000\147\000\098\000\061\000\
\061\000\000\000\061\000\000\000\027\000\028\000\120\000\000\000\
\061\000\061\000\061\000\061\000\000\000\029\000\030\000\056\000\
\031\000\061\000\061\000\056\000\056\000\000\000\056\000\056\000\
\056\000\056\000\056\000\000\000\056\000\056\000\056\000\056\000\
\056\000\056\000\056\000\056\000\057\000\056\000\056\000\000\000\
\057\000\057\000\000\000\057\000\057\000\057\000\000\000\000\000\
\000\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\058\000\057\000\057\000\000\000\058\000\058\000\000\000\
\058\000\058\000\058\000\000\000\000\000\000\000\058\000\058\000\
\058\000\058\000\058\000\058\000\058\000\058\000\141\000\058\000\
\058\000\000\000\000\000\000\000\000\000\000\000\053\000\054\000\
\055\000\056\000\000\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\144\000\065\000\066\000\000\000\000\000\
\000\000\000\000\000\000\053\000\054\000\055\000\056\000\000\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\063\000\065\000\066\000\000\000\063\000\063\000\000\000\063\000\
\000\000\000\000\000\000\000\000\000\000\063\000\063\000\063\000\
\063\000\063\000\063\000\063\000\063\000\064\000\063\000\063\000\
\000\000\064\000\064\000\000\000\064\000\000\000\000\000\000\000\
\000\000\000\000\064\000\064\000\064\000\064\000\064\000\064\000\
\064\000\064\000\065\000\064\000\064\000\000\000\065\000\065\000\
\000\000\065\000\000\000\000\000\000\000\000\000\000\000\065\000\
\065\000\065\000\065\000\065\000\065\000\065\000\065\000\066\000\
\065\000\065\000\000\000\066\000\066\000\000\000\066\000\000\000\
\000\000\000\000\000\000\000\000\066\000\066\000\066\000\066\000\
\066\000\066\000\066\000\066\000\062\000\066\000\066\000\000\000\
\062\000\062\000\000\000\062\000\000\000\000\000\000\000\000\000\
\000\000\062\000\062\000\062\000\062\000\000\000\000\000\000\000\
\067\000\000\000\062\000\062\000\067\000\067\000\000\000\067\000\
\000\000\000\000\000\000\068\000\000\000\067\000\067\000\068\000\
\068\000\000\000\068\000\000\000\000\000\000\000\067\000\067\000\
\068\000\068\000\000\000\000\000\000\000\000\000\000\000\000\000\
\074\000\068\000\068\000\053\000\054\000\055\000\056\000\000\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\000\000\065\000\066\000\052\000\053\000\054\000\055\000\056\000\
\000\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\000\000\065\000\066\000\132\000\053\000\054\000\055\000\
\056\000\000\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\000\000\065\000\066\000\140\000\053\000\054\000\
\055\000\056\000\000\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\000\000\065\000\066\000\053\000\054\000\
\055\000\056\000\000\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\000\000\065\000\066\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000"

let yycheck = "\024\000\
\001\001\026\000\010\001\011\001\012\001\013\001\081\000\001\000\
\096\000\054\001\001\001\019\001\020\001\021\001\022\001\012\001\
\013\001\002\001\043\000\044\000\108\000\006\001\007\001\014\001\
\009\001\054\001\001\001\054\001\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\117\000\005\001\054\001\003\001\004\001\005\001\
\054\001\074\000\051\001\052\001\054\001\054\001\045\001\046\001\
\047\001\048\001\049\001\050\001\002\001\136\000\054\001\002\001\
\006\001\007\001\002\001\009\001\026\001\027\001\006\001\007\001\
\030\001\009\001\032\001\100\000\007\001\003\001\004\001\005\001\
\042\001\043\001\009\001\002\001\042\001\043\001\044\001\014\001\
\007\001\051\001\052\001\116\000\054\001\051\001\052\001\120\000\
\054\001\006\001\002\001\001\001\026\001\027\001\127\000\007\001\
\030\001\130\000\032\001\003\001\004\001\005\001\001\001\003\001\
\010\001\011\001\012\001\013\001\042\001\043\001\044\001\017\001\
\018\001\019\001\020\001\021\001\022\001\051\001\052\001\002\001\
\054\001\007\001\026\001\027\001\002\001\002\001\030\001\009\001\
\032\001\003\001\004\001\005\001\001\001\001\001\144\000\145\000\
\146\000\004\001\042\001\043\001\044\001\151\000\152\000\153\000\
\031\001\054\001\031\001\051\001\052\001\028\001\054\001\002\001\
\026\001\027\001\051\001\052\001\030\001\054\001\032\001\003\001\
\004\001\005\001\035\001\036\001\037\001\038\001\039\001\040\001\
\042\001\043\001\044\001\035\001\036\001\037\001\038\001\039\001\
\040\001\051\001\052\001\028\001\054\001\029\001\026\001\027\001\
\029\001\000\000\030\001\002\001\032\001\003\001\006\001\005\001\
\002\001\036\001\037\001\038\001\039\001\040\001\042\001\043\001\
\044\001\010\001\011\001\012\001\013\001\006\001\002\001\051\001\
\052\001\002\001\054\001\009\001\026\001\027\001\004\001\009\001\
\030\001\114\000\032\001\002\001\073\000\143\000\074\000\006\001\
\007\001\255\255\009\001\255\255\042\001\043\001\044\001\255\255\
\015\001\016\001\017\001\018\001\255\255\051\001\052\001\002\001\
\054\001\024\001\025\001\006\001\007\001\255\255\009\001\010\001\
\011\001\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\002\001\024\001\025\001\255\255\
\006\001\007\001\255\255\009\001\010\001\011\001\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\002\001\024\001\025\001\255\255\006\001\007\001\255\255\
\009\001\010\001\011\001\255\255\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\002\001\024\001\
\025\001\255\255\255\255\255\255\255\255\255\255\010\001\011\001\
\012\001\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\002\001\024\001\025\001\255\255\255\255\
\255\255\255\255\255\255\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\002\001\024\001\025\001\255\255\006\001\007\001\255\255\009\001\
\255\255\255\255\255\255\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\002\001\024\001\025\001\
\255\255\006\001\007\001\255\255\009\001\255\255\255\255\255\255\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\002\001\024\001\025\001\255\255\006\001\007\001\
\255\255\009\001\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\002\001\
\024\001\025\001\255\255\006\001\007\001\255\255\009\001\255\255\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\002\001\024\001\025\001\255\255\
\006\001\007\001\255\255\009\001\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\255\255\255\255\255\255\
\002\001\255\255\024\001\025\001\006\001\007\001\255\255\009\001\
\255\255\255\255\255\255\002\001\255\255\015\001\016\001\006\001\
\007\001\255\255\009\001\255\255\255\255\255\255\024\001\025\001\
\015\001\016\001\255\255\255\255\255\255\255\255\255\255\255\255\
\007\001\024\001\025\001\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\009\001\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\009\001\010\001\011\001\012\001\
\013\001\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\009\001\010\001\011\001\
\012\001\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\010\001\011\001\
\012\001\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\034\001\035\001\
\036\001\037\001\038\001\039\001\040\001"

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
  PATHEXT\000\
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
# 445 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 453 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 461 "parser.ml"
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
# 477 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                ( VoidType )
# 483 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( IntType )
# 489 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                ( BoolType )
# 495 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                ( PathType )
# 501 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                ( StrType )
# 507 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                ( ListType )
# 513 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
    ( [] )
# 519 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 58 "parser.mly"
                    ( List.rev _1 )
# 526 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 61 "parser.mly"
                               ( [_1] )
# 533 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 62 "parser.mly"
                               ( _3 :: _1 )
# 541 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                  ( { vtype = IntType;  vname = _2; vexpr = Noexpr; } )
# 548 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                  ( { vtype = BoolType; vname = _2; vexpr = Noexpr; } )
# 555 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                  ( { vtype = PathType; vname = _2; vexpr = Noexpr; } )
# 562 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                  ( { vtype = StrType;  vname = _2; vexpr = Noexpr; } )
# 569 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                  ( { vtype = ListType; vname = _2; vexpr = Noexpr; } )
# 576 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
    ( [] )
# 582 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 74 "parser.mly"
                    ( List.rev _1 )
# 589 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 77 "parser.mly"
                       ( [_1] )
# 596 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 78 "parser.mly"
                       ( _2 :: _1 )
# 604 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 82 "parser.mly"
                            ( { vtype = _1;  vname = _2; vexpr = Noexpr } )
# 612 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'vdecl_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                                      ( { vtype = _1;  vname = _2; vexpr = _4 } )
# 621 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                    ( VoidType )
# 627 "parser.ml"
               : 'vdecl_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                    ( IntType )
# 633 "parser.ml"
               : 'vdecl_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
                    ( BoolType )
# 639 "parser.ml"
               : 'vdecl_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                    ( StrType )
# 645 "parser.ml"
               : 'vdecl_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                    ( PathType )
# 651 "parser.ml"
               : 'vdecl_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
                    ( ListType )
# 657 "parser.ml"
               : 'vdecl_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
    ( [] )
# 663 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 96 "parser.mly"
                     ( _2 :: _1 )
# 671 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 99 "parser.mly"
                       ( List.rev _1 )
# 678 "parser.ml"
               : 'rev_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                                                        ( Expr(_1) )
# 685 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 105 "parser.mly"
                                                        ( Return(_2) )
# 692 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                                                        ( If(_3, _6, Block([])) )
# 700 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                                        ( If(_3, _6, _8) )
# 709 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                                                        ( Print(_2) )
# 716 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 109 "parser.mly"
                                                        ( While(_3, _5) )
# 724 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'for_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'for_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 110 "parser.mly"
                                                        ( For(_3, _5, _7 ) )
# 733 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'list_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'list_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 111 "parser.mly"
                                                        ( Ifin(_2, _4, _6, Block([])) )
# 742 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'list_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'list_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 112 "parser.mly"
                                                        ( Ifin(_2, _4, _6, _8) )
# 752 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rev_stmt_list) in
    Obj.repr(
# 113 "parser.mly"
                                                        ( Block(_2) )
# 759 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                                    ( Forid(_1) )
# 766 "parser.ml"
               : 'for_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
                                   ( ListId(_1) )
# 773 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 121 "parser.mly"
                                   ( ListItemInt(_1) )
# 780 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
                                   ( ListItemStr(_1) )
# 787 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                  ( Noexpr )
# 793 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                  ( _1 )
# 800 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 129 "parser.mly"
                                   ( LitInt(_1) )
# 807 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
                                   ( LitInt(1) )
# 813 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                                   ( LitInt(0) )
# 819 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 132 "parser.mly"
                                   ( LitStr(_1) )
# 826 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_items) in
    Obj.repr(
# 133 "parser.mly"
                                   ( List(_2) )
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
                                   ( Id(_1) )
# 840 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 848 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 856 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 864 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 872 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 880 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 888 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 896 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 904 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 912 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 920 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                                   ( Binop(_1, And,      _3) )
# 928 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                                   ( Binop(_1, Or,       _3) )
# 936 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                                   ( Assign(_1, _3) )
# 944 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                                     ( Copy(_1,   _3) )
# 952 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                                     ( Move(_1,  _3) )
# 960 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 150 "parser.mly"
                                   ( Call(_1,   _3) )
# 968 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathattributes) in
    Obj.repr(
# 151 "parser.mly"
                                   ( Pathattr(_1, _2) )
# 976 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_expr) in
    Obj.repr(
# 152 "parser.mly"
                                     ( ListAppend(_1, _4) )
# 984 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_expr) in
    Obj.repr(
# 153 "parser.mly"
                                        ( ListRemove(_1, _4) )
# 992 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
                                   ( Pathname )
# 998 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
                                   ( Pathcreated )
# 1004 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "parser.mly"
                                   ( Pathkind )
# 1010 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "parser.mly"
                                   ( Pathext )
# 1016 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 162 "parser.mly"
    ( Noitem )
# 1022 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "parser.mly"
                                    ( Item(_1) )
# 1029 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_items) in
    Obj.repr(
# 164 "parser.mly"
                                   ( Seq(_1, Comma, _3) )
# 1037 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    Obj.repr(
# 168 "parser.mly"
                    ( [] )
# 1043 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 169 "parser.mly"
                    ( List.rev _1 )
# 1050 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "parser.mly"
                              ( [_1] )
# 1057 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 173 "parser.mly"
                              ( _3 :: _1 )
# 1065 "parser.ml"
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
