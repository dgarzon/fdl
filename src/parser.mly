%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA TAB SEMI
%token PLUS MINUS TIMES DIVIDE ASSIGN MOVE COPY 
%token EQ NEQ LT LEQ GT GEQ NOT
%token AND OR
%token RETURN IF THEN ELSE FOR IN WHILE DO
%token DEF VOID INT STR LIST PATH BOOL TRASH TRUE FALSE PRINT
%token PATHNAME PATHCREATED PATHKIND PATHEXT ADD REMOVE
%token <int> LIT_INT
%token <string> LIT_STR
%token <bool> LIT_BOOL
%token <string> ID
%token IN
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN MOVE COPY NOT

%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left IN
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
    { [], [] }
    | program vdecl { ($2 :: fst $1), snd $1 }
    | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
    DEF return_type ID LPAREN formals_opt RPAREN LBRACE vdecl_opt stmt_list RBRACE
       {{
        return = $2;
        fname = $3;
        formals = $5;
        fnlocals = List.rev $8;
        body = List.rev $9 }}

return_type:
      VOID      { VoidType }
    | INT       { IntType }
    | BOOL      { BoolType }
    | PATH      { PathType }
    | STR       { StrType }
    | LIST      { ListType }

formals_opt:
    { [] }
    | formal_list   { List.rev $1 }

formal_list:
    formal                     { [$1] }
    | formal_list COMMA formal { $3 :: $1 }

formal:
    INT ID        { { vtype = IntType;  vname = $2; vexpr = Noexpr; } }
    | BOOL ID     { { vtype = BoolType; vname = $2; vexpr = Noexpr; } }
    | PATH ID     { { vtype = PathType; vname = $2; vexpr = Noexpr; } }
    | STR ID      { { vtype = StrType;  vname = $2; vexpr = Noexpr; } }
    | LIST ID     { { vtype = ListType; vname = $2; vexpr = Noexpr; } }

/* Var declarations can also be optional */
vdecl_opt:
    { [] }
    | vdecl_list    { List.rev $1 }

vdecl_list:
    vdecl              { [$1] }
    | vdecl_list vdecl { $2 :: $1 }

/* Using SEMI to separate variable declarations for now */
vdecl:
      vdecl_type ID SEMI    { { vtype = $1;  vname = $2; vexpr = Noexpr } }
    | vdecl_type ID ASSIGN expr SEMI  { { vtype = $1;  vname = $2; vexpr = $4 } }
   
/* addded void type to variables so we can give this error in type checking*/
vdecl_type:
    VOID            { VoidType }
    | INT           { IntType }
    | BOOL          { BoolType }
    | STR           { StrType }
    | PATH          { PathType }
    | LIST          { ListType }

stmt_list:
    { [] }
    | stmt_list stmt { $2 :: $1 }

rev_stmt_list:
    stmt_list          { List.rev $1 }


/* using SEMI to separate stmts for now */
stmt:
    expr SEMI                                           { Expr($1) }
    | RETURN expr_opt SEMI                              { Return($2) }
    | IF LPAREN expr RPAREN THEN stmt %prec NOELSE      { If($3, $6, Block([])) }
    | IF LPAREN expr RPAREN THEN stmt ELSE stmt         { If($3, $6, $8) }
    | PRINT expr SEMI                                   { Print($2) }
    | WHILE LPAREN expr RPAREN stmt                     { While($3, $5) } 
    | FOR LPAREN for_expr IN for_expr RPAREN stmt       { For($3, $5, $7 ) } 
    | IF list_expr IN list_expr THEN stmt %prec NOELSE  { Ifin($2, $4, $6, Block([])) }
    | IF list_expr IN list_expr THEN stmt ELSE stmt     { Ifin($2, $4, $6, $8) }
    | LBRACE rev_stmt_list RBRACE                       { Block($2) }


for_expr:
    ID                              { Forid($1) }

list_expr:
    ID                             { ListId($1) }
    | LIT_INT                      { ListItemInt($1) }
    | LIT_STR                      { ListItemStr($1) }
/* expression optional, return; */
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    | LIT_INT                      { LitInt($1) }
    | TRUE                         { LitInt(1) }
    | FALSE                        { LitInt(0) }
    | LIT_STR                      { LitStr($1) }
    | LBRACK list_items RBRACK     { List($2) }
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
    | expr AND expr                { Binop($1, And,      $3) }
    | expr OR expr                 { Binop($1, Or,       $3) }
    | ID ASSIGN expr               { Assign($1, $3) }
    | expr COPY expr                 { Copy($1,   $3) }
    | expr MOVE expr                 { Move($1,  $3) }
    | ID LPAREN actuals_opt RPAREN { Call($1,   $3) }
    | ID pathattributes            { Pathattr($1, $2) }
    | ID ADD LPAREN list_expr RPAREN { ListAppend($1, $4) }
    | ID REMOVE LPAREN list_expr RPAREN { ListRemove($1, $4) }

pathattributes:
    | PATHNAME                     { Pathname }
    | PATHCREATED                  { Pathcreated }
    | PATHKIND                     { Pathkind }
    | PATHEXT                      { Pathext }

list_items:
    { Noitem }
    |  expr                         { Item($1) }
    | expr COMMA list_items        { Seq($1, Comma, $3) }   

    
actuals_opt:
    /* nothing */   { [] }
    | actuals_list  { List.rev $1 }

actuals_list:
    expr                      { [$1] }
    | actuals_list COMMA expr { $3 :: $1 }