%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN MOVE COPY
%token EQ NEQ LT LEQ GT GEQ NOT
%token AND OR
%token CONTINUE BREAK
%token RETURN IF THEN ELSE FOR IN WHILE
%token DEF VOID INT STR DICT LIST PATH BOOL TRASH
%token <int> LIT_INT
%token <string> LIT_STR
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN MOVE COPY NOT

%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
    VOID ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
       { {
          fname = $2;
          formals = $4;
          locals = List.rev $7;
          body = List.rev $8 } }
   | INT ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      { {
         return = int_type;
         fname = $2;
         formals = $4;
         locals = List.rev $7;
         body = List.rev $8 } }
   | STR ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      { {
         return = str_type;
         fname = $2;
         formals = $4;
         locals = List.rev $7;
         body = List.rev $8 } }
   | PATH ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      { {
         return = path_type;
         fname = $2;
         formals = $4;
         locals = List.rev $7;
         body = List.rev $8 } }
   | BOOL ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      { {
         return = bool_type;
         fname = $2;
         formals = $4;
         locals = List.rev $7;
         body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    formal                   { [$1] }
  | formal_list COMMA formal { $3 :: $1 }

formal:
    INT ID   { { var_type = int_type; name = $2; } }
  | BOOL ID { { var_type = bool_type; name = $2; } }
  | PATH ID { { var_type = path_type; name = $2; } }
  | STR ID { { var_type = str_type; name = $2; } }
  | DICT ID  { { var_type = dict_type; name = $2; } }
  | LIST ID  { { var_type = list_type; name = $2; } }


vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
      INT ID    {{var_type = Ast.Int;  var_name = $2; data_type = Ast.Int}}
    | BOOL ID   {{var_type = Ast.Bool; var_name = $2; data_type = Ast.Bool}}
    | STR ID    {{var_type = Ast.Str;  var_name = $2; data_type = Ast.Str}}
    | PATH ID   {{var_type = Ast.Path; var_name = $2; data_type = Ast.Path}}
    | DICT ID   {{var_type = Ast.Dict; var_name = $2; data_type = Ast.Dict}}
    | LIST ID   {{var_type = Ast.List; var_name = $2; data_type = Ast.List}}

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr { Expr($1) }
  | RETURN expr                                  { Return($2) }
  | LBRACE stmt_list RBRACE                      { Block(List.rev $2) }
  | IF LPAREN expr RPAREN THEN stmt %prec NOELSE { If($3, $6, Block([])) }
  | IF LPAREN expr RPAREN THEN stmt ELSE stmt    { If($3, $6, $8) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
  | LIT_INT                      { LitInt($1) }
  | LIT_STR                      { LitStr($1) }
  | ID                           { Id($1) }
  | expr PLUS   expr             { Binop($1, Add,      $3) }
  | expr MINUS  expr             { Binop($1, Sub,      $3) }
  | expr TIMES  expr             { Binop($1, Mult,     $3) }
  | expr DIVIDE expr             { Binop($1, Div,      $3) }
  | expr EQ     expr             { Binop($1, Equal,    $3) }
  | expr NEQ    expr             { Binop($1, Neq,      $3) }
  | expr LT     expr             { Binop($1, Less,     $3) }
  | expr LEQ    expr             { Binop($1, Leq,      $3) }
  | expr GT     expr             { Binop($1, Greater,  $3) }
  | expr GEQ    expr             { Binop($1, Geq,      $3) }
  | ID ASSIGN expr               { Assign($1, $3) }
  | ID COPY expr                 { Copy($1,   $3) }
  | ID MOVE expr                 { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1,   $3) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

