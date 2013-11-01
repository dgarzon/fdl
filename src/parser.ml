type token =
  | SEMI
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
  | LESS
  | GTR
  | AND
  | OR
  | CONTINUE
  | BREAK
  | DEF
  | RETURN
  | IF
  | THEN
  | ELSE
  | FOR
  | IN
  | WHILE
  | INT
  | STR
  | DICT
  | LIST
  | PATH
  | TRASH
  | MAIN
  | LIT_INT of (int)
  | LIT_STR of (string)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 54 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* COMMA *);
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
  278 (* LESS *);
  279 (* GTR *);
  280 (* AND *);
  281 (* OR *);
  282 (* CONTINUE *);
  283 (* BREAK *);
  284 (* DEF *);
  285 (* RETURN *);
  286 (* IF *);
  287 (* THEN *);
  288 (* ELSE *);
  289 (* FOR *);
  290 (* IN *);
  291 (* WHILE *);
  292 (* INT *);
  293 (* STR *);
  294 (* DICT *);
  295 (* LIST *);
  296 (* PATH *);
  297 (* TRASH *);
  298 (* MAIN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  299 (* LIT_INT *);
  300 (* LIT_STR *);
  301 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\006\000\006\000\007\000\007\000\007\000\007\000\
\007\000\002\000\002\000\002\000\002\000\002\000\005\000\005\000\
\008\000\008\000\008\000\008\000\008\000\008\000\009\000\009\000\
\009\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\006\000\006\000\006\000\006\000\006\000\
\000\000\001\000\001\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\000\000\002\000\
\001\000\002\000\001\000\006\000\008\000\005\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\000\000\000\000\000\000\000\000\000\
\013\000\014\000\016\000\017\000\015\000\023\000\000\000\023\000\
\023\000\023\000\023\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\031\000\032\000\033\000\000\000\
\024\000\025\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\030\000\028\000\000\000\029\000"

let yydgoto = "\002\000\
\003\000\009\000\010\000\026\000\056\000\027\000\028\000\057\000\
\058\000"

let yysindex = "\255\255\
\000\000\000\000\016\255\216\254\218\254\219\254\221\254\225\254\
\000\000\000\000\013\255\025\255\028\255\040\255\041\255\021\255\
\021\255\021\255\021\255\021\255\255\254\000\255\002\255\003\255\
\004\255\047\255\054\255\000\000\060\255\061\255\062\255\063\255\
\000\000\000\000\000\000\000\000\000\000\000\000\021\255\000\000\
\000\000\000\000\000\000\230\254\000\000\230\254\230\254\230\254\
\230\254\246\254\065\255\066\255\000\000\000\000\000\000\230\254\
\000\000\000\000\000\000\246\254\246\254\067\255\068\255\038\255\
\230\254\230\254\000\000\000\000\230\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\072\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\011\000\016\000\021\000\026\000\070\255\
\070\255\070\255\070\255\070\255\000\000\000\000\000\000\000\000\
\000\000\000\000\071\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\000\000\036\000\041\000\046\000\
\051\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\001\000\000\000\000\000\001\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\020\000\238\255\000\000\037\000\219\255\
\208\255"

let yytablesize = 347
let yytable = "\001\000\
\023\000\059\000\050\000\051\000\011\000\018\000\012\000\013\000\
\052\000\014\000\019\000\062\000\063\000\015\000\016\000\021\000\
\053\000\054\000\055\000\044\000\022\000\046\000\047\000\048\000\
\049\000\020\000\017\000\067\000\068\000\018\000\004\000\070\000\
\053\000\054\000\055\000\005\000\029\000\030\000\031\000\032\000\
\007\000\019\000\020\000\033\000\034\000\008\000\035\000\036\000\
\037\000\038\000\006\000\004\000\005\000\006\000\007\000\008\000\
\021\000\022\000\023\000\024\000\025\000\039\000\040\000\041\000\
\042\000\043\000\060\000\061\000\066\000\064\000\065\000\034\000\
\009\000\010\000\000\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\000\000\000\000\000\000\023\000\023\000\023\000\023\000\
\023\000\018\000\018\000\018\000\018\000\018\000\019\000\019\000\
\019\000\019\000\019\000\021\000\021\000\021\000\021\000\021\000\
\022\000\022\000\022\000\022\000\022\000\020\000\020\000\020\000\
\020\000\020\000\004\000\004\000\004\000\004\000\004\000\005\000\
\005\000\005\000\005\000\005\000\007\000\007\000\007\000\007\000\
\007\000\008\000\008\000\008\000\008\000\008\000\006\000\006\000\
\006\000\006\000\006\000"

let yycheck = "\001\000\
\000\000\050\000\029\001\030\001\045\001\000\000\045\001\045\001\
\035\001\045\001\000\000\060\000\061\000\045\001\002\001\000\000\
\043\001\044\001\045\001\038\000\000\000\040\000\041\000\042\000\
\043\000\000\000\002\001\065\000\066\000\002\001\000\000\069\000\
\043\001\044\001\045\001\000\000\017\000\018\000\019\000\020\000\
\000\000\002\001\002\001\045\001\045\001\000\000\045\001\045\001\
\045\001\003\001\000\000\036\001\037\001\038\001\039\001\040\001\
\036\001\037\001\038\001\039\001\040\001\008\001\003\001\003\001\
\003\001\003\001\002\001\002\001\031\001\003\001\003\001\000\000\
\003\001\003\001\255\255\039\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\032\001\255\255\255\255\255\255\036\001\037\001\038\001\039\001\
\040\001\036\001\037\001\038\001\039\001\040\001\036\001\037\001\
\038\001\039\001\040\001\036\001\037\001\038\001\039\001\040\001\
\036\001\037\001\038\001\039\001\040\001\036\001\037\001\038\001\
\039\001\040\001\036\001\037\001\038\001\039\001\040\001\036\001\
\037\001\038\001\039\001\040\001\036\001\037\001\038\001\039\001\
\040\001\036\001\037\001\038\001\039\001\040\001\036\001\037\001\
\038\001\039\001\040\001"

let yynames_const = "\
  SEMI\000\
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
  LESS\000\
  GTR\000\
  AND\000\
  OR\000\
  CONTINUE\000\
  BREAK\000\
  DEF\000\
  RETURN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FOR\000\
  IN\000\
  WHILE\000\
  INT\000\
  STR\000\
  DICT\000\
  LIST\000\
  PATH\000\
  TRASH\000\
  MAIN\000\
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
# 32 "parser.mly"
                 ( [], [] )
# 313 "parser.ml"
               : Ast.main))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.main) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 33 "parser.mly"
                   ( (_2 :: fst _1), snd _1 )
# 321 "parser.ml"
               : Ast.main))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.main) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 34 "parser.mly"
                   ( fst _1, (_2 :: snd _1) )
# 329 "parser.ml"
               : Ast.main))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 38 "parser.mly"
        ({
            return = literal_int;
            function_name = _2;
            parameters = _4;
            body = _6;
        })
# 343 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 45 "parser.mly"
        ({
            return = literal_str;
            function_name = _2;
            parameters = _4;
            body = List.rev _6;
        })
# 357 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 52 "parser.mly"
        ({
            return = path_type;
            function_name = _2;
            parameters = _4;
            body = List.rev _6;
        })
# 371 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 59 "parser.mly"
        ({
            return = dict_type;
            function_name = _2;
            parameters = _4;
            body = List.rev _6;
        })
# 385 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 66 "parser.mly"
        ({
            return = list_type;
            function_name = _2;
            parameters = _4;
            body = List.rev _6;
        })
# 399 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
    ( [] )
# 405 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 75 "parser.mly"
                     ( List.rev _1 )
# 412 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 78 "parser.mly"
                                    ( [_1] )
# 419 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 79 "parser.mly"
                                    ( _3 :: _1 )
# 427 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
                ({var_type = lit_int; var_name = _2})
# 434 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                ({var_type = lit_str; var_name = _2})
# 441 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                ({var_type = path_type; var_name = _2})
# 448 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                ({var_type = dict_type; var_name = _2})
# 455 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                ({var_type = list_type; var_name = _2})
# 462 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
                ({var_type = Ast.Int; var_name = _2;  data_type = Ast.Int})
# 469 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
                ({var_type = Ast.Str; var_name = _2; data_type = Ast.Str})
# 476 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
                ({var_type = Ast.Path; var_name = _2; data_type = Ast.Path})
# 483 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                ({var_type = Ast.Dict; var_name = _2; data_type = Ast.Dict})
# 490 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
                ({var_type = Ast.List; var_name = _2; data_type = Ast.List})
# 497 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
    ( [] )
# 503 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 97 "parser.mly"
                     ( _2 :: _1 )
# 511 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                                                    ( Expr(_1) )
# 518 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                                                    ( Return(_2) )
# 525 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 102 "parser.mly"
                                                    ( Block(List.rev _1) )
# 532 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
                                                    ( If(_3, _6, Block([])) )
# 540 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                                    ( If(_3, _6, _8) )
# 549 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                                                    ( While(_3, _5) )
# 557 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 108 "parser.mly"
                ( Literal(_1) )
# 564 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                ( Literal(_1) )
# 571 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
                ( Id(_1) )
# 578 "parser.ml"
               : 'expr))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.main)
