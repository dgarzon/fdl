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
  296 (* LIST *);
  297 (* PATH *);
  298 (* BOOL *);
  299 (* TRASH *);
  300 (* TRUE *);
  301 (* FALSE *);
  302 (* PRINT *);
  303 (* PATHNAME *);
  304 (* PATHCREATED *);
  305 (* PATHKIND *);
  306 (* PATHEXT *);
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
\004\000\004\000\005\000\005\000\008\000\008\000\009\000\009\000\
\009\000\009\000\009\000\006\000\006\000\010\000\010\000\002\000\
\002\000\002\000\002\000\002\000\002\000\007\000\007\000\012\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\015\000\016\000\016\000\016\000\016\000\014\000\
\014\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\019\000\019\000\019\000\019\000\017\000\017\000\017\000\018\000\
\018\000\020\000\020\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\010\000\001\000\001\000\001\000\001\000\
\001\000\001\000\000\000\001\000\001\000\003\000\002\000\002\000\
\002\000\002\000\002\000\000\000\001\000\001\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\000\000\002\000\001\000\
\002\000\003\000\006\000\008\000\003\000\005\000\007\000\006\000\
\008\000\003\000\001\000\001\000\001\000\001\000\001\000\000\000\
\001\000\001\000\001\000\001\000\003\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\002\000\005\000\005\000\
\001\000\001\000\001\000\001\000\000\000\001\000\003\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\003\000\005\000\006\000\009\000\010\000\
\008\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\025\000\027\000\029\000\028\000\026\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\015\000\018\000\019\000\017\000\016\000\000\000\000\000\
\000\000\014\000\022\000\030\000\000\000\000\000\023\000\030\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\050\000\
\051\000\052\000\000\000\031\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\000\046\000\047\000\044\000\
\000\000\000\000\000\000\000\000\000\000\000\000\073\000\074\000\
\075\000\076\000\000\000\000\000\070\000\033\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\042\000\000\000\053\000\034\000\000\000\
\000\000\043\000\000\000\000\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\057\000\058\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\079\000\000\000\000\000\000\000\000\000\069\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\000\000\000\071\000\072\000\
\000\000\000\000\000\000\000\000\000\000\039\000\036\000\041\000"

let yydgoto = "\002\000\
\003\000\011\000\012\000\019\000\039\000\052\000\054\000\040\000\
\041\000\053\000\068\000\071\000\069\000\075\000\115\000\081\000\
\073\000\119\000\093\000\120\000"

let yysindex = "\005\000\
\000\000\000\000\246\000\164\255\208\254\214\254\215\254\217\254\
\223\254\232\254\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\234\254\041\255\043\255\044\255\052\255\059\255\
\077\255\095\255\000\000\000\000\000\000\000\000\000\000\000\000\
\153\255\049\255\050\255\051\255\060\255\061\255\097\255\090\255\
\000\000\000\000\000\000\000\000\000\000\000\000\115\255\153\255\
\241\255\000\000\000\000\000\000\241\255\055\255\000\000\000\000\
\000\000\019\255\019\255\001\255\122\255\125\255\019\255\000\000\
\000\000\000\000\015\255\000\000\102\000\154\255\120\255\083\000\
\128\255\140\000\126\255\019\255\000\000\000\000\000\000\000\000\
\104\255\093\255\019\255\121\000\019\255\019\255\000\000\000\000\
\000\000\000\000\149\255\150\255\000\000\000\000\019\255\019\255\
\019\255\019\255\019\255\019\255\019\255\019\255\019\255\019\255\
\019\255\019\255\019\255\000\000\019\255\000\000\000\000\019\000\
\024\255\000\000\127\255\043\000\000\000\140\000\156\255\155\255\
\140\000\024\255\024\255\018\255\018\255\000\000\000\000\140\000\
\140\000\000\255\000\255\157\255\157\255\157\255\157\255\036\000\
\000\000\131\255\146\255\093\255\154\255\000\000\019\255\171\255\
\178\255\154\255\154\255\182\255\000\000\140\000\000\000\000\000\
\181\255\183\255\154\255\154\255\154\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\185\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\185\255\000\000\000\000\000\000\000\000\000\000\000\000\211\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\066\255\000\000\000\000\000\000\099\255\000\000\000\000\000\000\
\000\000\217\255\208\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\209\255\000\000\000\000\229\255\000\000\228\255\
\000\000\226\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\236\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\217\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\255\000\000\242\255\
\123\255\000\000\000\000\230\255\251\255\000\000\000\000\134\255\
\172\255\245\000\006\001\161\000\182\000\203\000\224\000\064\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\086\255\000\000\000\000\
\110\255\143\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\210\255\000\000\000\000\000\000\000\000\187\000\000\000\
\206\000\000\000\191\255\000\000\198\255\000\000\115\000\169\255\
\147\000\000\000\000\000\000\000"

let yytablesize = 544
let yytable = "\072\000\
\074\000\076\000\051\000\082\000\084\000\001\000\055\000\020\000\
\082\000\095\000\096\000\097\000\098\000\021\000\022\000\085\000\
\023\000\112\000\103\000\104\000\105\000\106\000\024\000\058\000\
\116\000\139\000\118\000\121\000\086\000\097\000\098\000\025\000\
\107\000\026\000\144\000\145\000\124\000\125\000\126\000\127\000\
\128\000\129\000\130\000\131\000\132\000\133\000\134\000\135\000\
\136\000\027\000\072\000\028\000\029\000\077\000\078\000\079\000\
\080\000\056\000\057\000\058\000\030\000\087\000\088\000\089\000\
\090\000\091\000\092\000\031\000\020\000\020\000\020\000\064\000\
\065\000\066\000\067\000\149\000\077\000\078\000\079\000\080\000\
\153\000\154\000\059\000\060\000\150\000\032\000\061\000\083\000\
\062\000\158\000\159\000\160\000\083\000\020\000\020\000\033\000\
\048\000\020\000\047\000\020\000\063\000\021\000\021\000\021\000\
\042\000\043\000\044\000\064\000\065\000\066\000\067\000\020\000\
\035\000\035\000\035\000\045\000\046\000\049\000\020\000\020\000\
\020\000\020\000\082\000\108\000\066\000\083\000\021\000\021\000\
\066\000\066\000\021\000\066\000\021\000\110\000\111\000\068\000\
\113\000\035\000\035\000\068\000\068\000\035\000\068\000\035\000\
\021\000\040\000\040\000\040\000\114\000\122\000\123\000\021\000\
\021\000\021\000\021\000\035\000\056\000\142\000\058\000\140\000\
\146\000\143\000\035\000\035\000\035\000\035\000\095\000\096\000\
\097\000\098\000\040\000\040\000\151\000\067\000\040\000\147\000\
\040\000\067\000\067\000\152\000\067\000\059\000\060\000\155\000\
\084\000\061\000\011\000\062\000\040\000\107\000\034\000\035\000\
\036\000\037\000\038\000\040\000\040\000\040\000\040\000\063\000\
\013\000\014\000\015\000\016\000\017\000\018\000\064\000\065\000\
\066\000\067\000\054\000\156\000\012\000\157\000\054\000\054\000\
\048\000\054\000\054\000\054\000\054\000\054\000\077\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\055\000\
\032\000\078\000\049\000\055\000\055\000\080\000\055\000\055\000\
\055\000\054\000\070\000\081\000\055\000\055\000\055\000\055\000\
\055\000\055\000\055\000\055\000\056\000\050\000\148\000\137\000\
\056\000\056\000\000\000\056\000\056\000\056\000\055\000\000\000\
\000\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
\056\000\000\000\000\000\000\000\138\000\005\000\006\000\007\000\
\008\000\009\000\010\000\056\000\095\000\096\000\097\000\098\000\
\000\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\106\000\000\000\000\000\000\000\141\000\095\000\096\000\097\000\
\098\000\000\000\000\000\107\000\095\000\096\000\097\000\098\000\
\000\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\106\000\059\000\000\000\000\000\000\000\059\000\059\000\000\000\
\059\000\000\000\000\000\107\000\000\000\000\000\059\000\059\000\
\059\000\059\000\059\000\059\000\059\000\059\000\000\000\000\000\
\000\000\109\000\000\000\000\000\095\000\096\000\097\000\098\000\
\059\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\106\000\000\000\000\000\000\000\000\000\000\000\094\000\095\000\
\096\000\097\000\098\000\107\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\000\000\000\000\000\000\000\000\
\000\000\117\000\095\000\096\000\097\000\098\000\107\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\000\000\
\000\000\000\000\000\000\000\000\000\000\095\000\096\000\097\000\
\098\000\107\000\099\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\062\000\000\000\000\000\000\000\062\000\062\000\
\000\000\062\000\000\000\000\000\107\000\000\000\000\000\062\000\
\062\000\062\000\062\000\062\000\062\000\062\000\062\000\063\000\
\000\000\000\000\000\000\063\000\063\000\000\000\063\000\000\000\
\000\000\000\000\000\000\000\000\063\000\063\000\063\000\063\000\
\063\000\063\000\063\000\063\000\064\000\000\000\000\000\000\000\
\064\000\064\000\000\000\064\000\000\000\000\000\000\000\000\000\
\000\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
\064\000\065\000\000\000\000\000\000\000\065\000\065\000\000\000\
\065\000\000\000\000\000\000\000\000\000\000\000\065\000\065\000\
\065\000\065\000\065\000\065\000\065\000\065\000\060\000\000\000\
\000\000\000\000\060\000\060\000\000\000\060\000\000\000\000\000\
\000\000\000\000\000\000\060\000\060\000\060\000\060\000\061\000\
\000\000\000\000\000\000\061\000\061\000\000\000\061\000\000\000\
\000\000\000\000\000\000\000\000\061\000\061\000\061\000\061\000\
\000\000\004\000\005\000\006\000\007\000\008\000\009\000\010\000"

let yycheck = "\058\000\
\059\000\001\001\049\000\002\001\063\000\001\000\053\000\056\001\
\007\001\010\001\011\001\012\001\013\001\056\001\056\001\001\001\
\056\001\076\000\019\001\020\001\021\001\022\001\056\001\005\001\
\083\000\113\000\085\000\086\000\014\001\012\001\013\001\056\001\
\033\001\056\001\122\000\123\000\095\000\096\000\097\000\098\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\107\000\009\001\109\000\009\001\009\001\053\001\054\001\055\001\
\056\001\003\001\004\001\005\001\009\001\047\001\048\001\049\001\
\050\001\051\001\052\001\009\001\003\001\004\001\005\001\053\001\
\054\001\055\001\056\001\141\000\053\001\054\001\055\001\056\001\
\146\000\147\000\028\001\029\001\143\000\009\001\032\001\002\001\
\034\001\155\000\156\000\157\000\007\001\028\001\029\001\001\001\
\007\001\032\001\002\001\034\001\046\001\003\001\004\001\005\001\
\056\001\056\001\056\001\053\001\054\001\055\001\056\001\046\001\
\003\001\004\001\005\001\056\001\056\001\003\001\053\001\054\001\
\055\001\056\001\001\001\004\001\002\001\001\001\028\001\029\001\
\006\001\007\001\032\001\009\001\034\001\006\001\009\001\002\001\
\033\001\028\001\029\001\006\001\007\001\032\001\009\001\034\001\
\046\001\003\001\004\001\005\001\056\001\001\001\001\001\053\001\
\054\001\055\001\056\001\046\001\003\001\002\001\005\001\033\001\
\030\001\007\001\053\001\054\001\055\001\056\001\010\001\011\001\
\012\001\013\001\028\001\029\001\002\001\002\001\032\001\030\001\
\034\001\006\001\007\001\002\001\009\001\028\001\029\001\002\001\
\000\000\032\001\002\001\034\001\046\001\033\001\038\001\039\001\
\040\001\041\001\042\001\053\001\054\001\055\001\056\001\046\001\
\037\001\038\001\039\001\040\001\041\001\042\001\053\001\054\001\
\055\001\056\001\002\001\031\001\002\001\031\001\006\001\007\001\
\009\001\009\001\010\001\011\001\012\001\013\001\006\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\002\001\
\004\001\006\001\009\001\006\001\007\001\002\001\009\001\010\001\
\011\001\033\001\056\000\002\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\002\001\048\000\140\000\109\000\
\006\001\007\001\255\255\009\001\010\001\011\001\033\001\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\002\001\037\001\038\001\039\001\
\040\001\041\001\042\001\033\001\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\002\001\010\001\011\001\012\001\
\013\001\255\255\255\255\033\001\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\002\001\255\255\255\255\255\255\006\001\007\001\255\255\
\009\001\255\255\255\255\033\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\255\255\
\255\255\007\001\255\255\255\255\010\001\011\001\012\001\013\001\
\033\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\255\255\255\255\009\001\010\001\
\011\001\012\001\013\001\033\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\009\001\010\001\011\001\012\001\013\001\033\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\255\255\255\255\255\255\255\255\255\255\010\001\011\001\012\001\
\013\001\033\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\002\001\255\255\255\255\255\255\006\001\007\001\
\255\255\009\001\255\255\255\255\033\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\002\001\
\255\255\255\255\255\255\006\001\007\001\255\255\009\001\255\255\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\002\001\255\255\255\255\255\255\
\006\001\007\001\255\255\009\001\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\002\001\255\255\255\255\255\255\006\001\007\001\255\255\
\009\001\255\255\255\255\255\255\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\002\001\255\255\
\255\255\255\255\006\001\007\001\255\255\009\001\255\255\255\255\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\002\001\
\255\255\255\255\255\255\006\001\007\001\255\255\009\001\255\255\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\255\255\036\001\037\001\038\001\039\001\040\001\041\001\042\001"

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
# 441 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "parser.mly"
                    ( (_2 :: fst _1), snd _1 )
# 449 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "parser.mly"
                    ( fst _1, (_2 :: snd _1) )
# 457 "parser.ml"
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
# 473 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                ( VoidType )
# 479 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( IntType )
# 485 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                ( BoolType )
# 491 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                ( PathType )
# 497 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                ( StrType )
# 503 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                ( ListType )
# 509 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
    ( [] )
# 515 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 58 "parser.mly"
                    ( List.rev _1 )
# 522 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 61 "parser.mly"
                               ( [_1] )
# 529 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 62 "parser.mly"
                               ( _3 :: _1 )
# 537 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                  ( { vtype = IntType;  vname = _2; } )
# 544 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                  ( { vtype = BoolType; vname = _2; } )
# 551 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                  ( { vtype = PathType; vname = _2; } )
# 558 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                  ( { vtype = StrType;  vname = _2; } )
# 565 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                  ( { vtype = ListType; vname = _2; } )
# 572 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
    ( [] )
# 578 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 74 "parser.mly"
                    ( List.rev _1 )
# 585 "parser.ml"
               : 'vdecl_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 77 "parser.mly"
                       ( [_1] )
# 592 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 78 "parser.mly"
                       ( _2 :: _1 )
# 600 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 83 "parser.mly"
                    ( { vtype = VoidType;  vname = _2; } )
# 607 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "parser.mly"
                     ( { vtype = IntType;  vname = _2; } )
# 614 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 85 "parser.mly"
                   ( { vtype = BoolType; vname = _2; } )
# 621 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
                   ( { vtype = StrType;  vname = _2; } )
# 628 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parser.mly"
                   ( { vtype = PathType; vname = _2; } )
# 635 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 88 "parser.mly"
                   ( { vtype = ListType; vname = _2; } )
# 642 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
    ( [] )
# 648 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 92 "parser.mly"
                     ( _2 :: _1 )
# 656 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 95 "parser.mly"
                       ( List.rev _1 )
# 663 "parser.ml"
               : 'rev_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                                                   ( Expr(_1) )
# 670 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 100 "parser.mly"
                                                   ( Return(_2) )
# 677 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "parser.mly"
                                                   ( If(_3, _6, Block([])) )
# 685 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "parser.mly"
                                                   ( If(_3, _6, _8) )
# 694 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                                                   ( Print(_2) )
# 701 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                                              ( While(_3, _5) )
# 709 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'for_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'for_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                                                                ( For(_3, _5, _7 ) )
# 718 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'list_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'list_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                                                        ( Ifin(_2, _4, _6, Block([])) )
# 727 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'list_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'list_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                                        ( Ifin(_2, _4, _6, _8) )
# 737 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rev_stmt_list) in
    Obj.repr(
# 108 "parser.mly"
                                                        ( Block(_2) )
# 744 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
                                    ( Forid(_1) )
# 751 "parser.ml"
               : 'for_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parser.mly"
                                  ( ListId(_1) )
# 758 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 115 "parser.mly"
                                   ( ListItemInt(_1) )
# 765 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
                                   ( ListItemStr(_1) )
# 772 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 117 "parser.mly"
                                   ( ListItemBool(_1) )
# 779 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
                  ( Noexpr )
# 785 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                  ( _1 )
# 792 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 124 "parser.mly"
                                   ( LitInt(_1) )
# 799 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 125 "parser.mly"
                                   ( LitStr(_1) )
# 806 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 126 "parser.mly"
                                                 ( LitBool(_1) )
# 813 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_items) in
    Obj.repr(
# 127 "parser.mly"
                                   ( List(_2) )
# 820 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Id(_1) )
# 827 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                   ( Binop(_1, Add,      _3) )
# 835 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                   ( Binop(_1, Sub,      _3) )
# 843 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                                   ( Binop(_1, Mult,     _3) )
# 851 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                                   ( Binop(_1, Div,      _3) )
# 859 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                                   ( Binop(_1, In,       _3) )
# 867 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                                   ( Binop(_1, Equal,    _3) )
# 875 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                                   ( Binop(_1, Neq,      _3) )
# 883 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                                   ( Binop(_1, Less,     _3) )
# 891 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                                   ( Binop(_1, Leq,      _3) )
# 899 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                                   ( Binop(_1, Greater,  _3) )
# 907 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                                   ( Binop(_1, Geq,      _3) )
# 915 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                                   ( Assign(_1, _3) )
# 923 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                                     ( Copy(_1,   _3) )
# 931 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                                     ( Move(_1,  _3) )
# 939 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 143 "parser.mly"
                                   ( Call(_1,   _3) )
# 947 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathattributes) in
    Obj.repr(
# 144 "parser.mly"
                                   ( Pathattr(_1, _2) )
# 955 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_expr) in
    Obj.repr(
# 145 "parser.mly"
                                     ( ListAppend(_1, _4) )
# 963 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_expr) in
    Obj.repr(
# 146 "parser.mly"
                                        ( ListRemove(_1, _4) )
# 971 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
                                   ( Pathname )
# 977 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 150 "parser.mly"
                                   ( Pathcreated )
# 983 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "parser.mly"
                                   ( Pathkind )
# 989 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
                                   ( Pathext )
# 995 "parser.ml"
               : 'pathattributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "parser.mly"
    ( Noitem )
# 1001 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
                                    ( Item(_1) )
# 1008 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_items) in
    Obj.repr(
# 157 "parser.mly"
                                   ( Seq(_1, Comma, _3) )
# 1016 "parser.ml"
               : 'list_items))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "parser.mly"
                    ( [] )
# 1022 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 162 "parser.mly"
                    ( List.rev _1 )
# 1029 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                              ( [_1] )
# 1036 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
                              ( _3 :: _1 )
# 1044 "parser.ml"
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
