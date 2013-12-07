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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
