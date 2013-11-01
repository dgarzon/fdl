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
\001\000\000\000"

let yylen = "\002\000\
\000\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\002\000"

let yydgoto = "\002\000\
\003\000"

let yysindex = "\255\255\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000"

let yytablesize = 0
let yytable = "\001\000"

let yycheck = "\001\000"

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
# 191 "parser.ml"
               : Ast.program))
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
