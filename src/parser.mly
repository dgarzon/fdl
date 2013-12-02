%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA TAB SEMI
%token PLUS MINUS TIMES DIVIDE IN ASSIGN MOVE COPY
%token EQ NEQ LT LEQ GT GEQ NOT
%token AND OR
%token CONTINUE BREAK
%token RETURN IF THEN ELSE FOR IN WHILE DO
%token DEF VOID INT STR DICT LIST PATH BOOL TRASH TRUE FALSE PRINT
%token PATHNAME PATHCREATED PATHKIND
%token <int> LIT_INT
%token <string> LIT_STR
%token <bool> LIT_BOOL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN MOVE COPY NOT

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
    | DICT      { DictType }
    | LIST      { ListType }

formals_opt:
    { [] }
    | formal_list   { List.rev $1 }

formal_list:
    formal                     { [$1] }
    | formal_list COMMA formal { $3 :: $1 }

formal:
    INT ID        { { vtype = IntType;  vname = $2; } }
    | BOOL ID     { { vtype = BoolType; vname = $2; } }
    | PATH ID     { { vtype = PathType; vname = $2; } }
    | STR ID      { { vtype = StrType;  vname = $2; } }
    | DICT ID     { { vtype = DictType; vname = $2; } }
    | LIST ID     { { vtype = ListType; vname = $2; } }

/* Var declarations can also be optional */
vdecl_opt:
    { [] }
    | vdecl_list    { List.rev $1 }

vdecl_list:
    vdecl              { [$1] }
    | vdecl_list vdecl { $2 :: $1 }

/* Using SEMI to separate variable declarations for now */
vdecl:
/* addded void type to variables so we can give this error in type checking*/
    VOID ID SEMI    { { vtype = VoidType;  vname = $2; } }
    | INT ID SEMI    { { vtype = IntType;  vname = $2; } }
    | BOOL ID SEMI { { vtype = BoolType; vname = $2; } }
    | STR ID SEMI  { { vtype = StrType;  vname = $2; } }
    | PATH ID SEMI { { vtype = PathType; vname = $2; } }
    | DICT ID SEMI { { vtype = DictType; vname = $2; } }
    | LIST ID SEMI { { vtype = ListType; vname = $2; } }

stmt_list:
    { [] }
    | stmt_list stmt { $2 :: $1 }

/* using SEMI to separate stmts for now */
stmt:
    expr SEMI                                      { Expr($1) }
    | RETURN expr_opt SEMI                         { Return($2) }
    | IF LPAREN expr RPAREN THEN stmt %prec NOELSE { If($3, $6, Block([])) }
    | IF LPAREN expr RPAREN THEN stmt ELSE stmt    { If($3, $6, $8) }
    | PRINT expr SEMI                              { Print($2) }
    | WHILE LPAREN expr RPAREN stmt 	   	   { While($3, Block([$5])) } 

/* expression optional, return; */
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    | LIT_INT                      { LitInt($1) }
    | LIT_STR                      { LitStr($1) }
    | LIT_BOOL			   { LitBool($1) }
    | LBRACK list_items RBRACK     { List($2) }
    | ID                           { Id($1) }
    | expr PLUS   expr             { Binop($1, Add,      $3) }
    | expr MINUS  expr             { Binop($1, Sub,      $3) }
    | expr TIMES  expr             { Binop($1, Mult,     $3) }
    | expr DIVIDE expr             { Binop($1, Div,      $3) }
    | expr IN expr                 { Binop($1, In,       $3) }
    | expr EQ     expr             { Binop($1, Equal,    $3) }
    | expr NEQ    expr             { Binop($1, Neq,      $3) }
    | expr LT     expr             { Binop($1, Less,     $3) }
    | expr LEQ    expr             { Binop($1, Leq,      $3) }
    | expr GT     expr             { Binop($1, Greater,  $3) }
    | expr GEQ    expr             { Binop($1, Geq,      $3) }
    | ID ASSIGN expr               { Assign($1, $3) }
    | expr COPY expr                 { Copy($1,   $3) }
    | expr MOVE expr                 { Move($1,  $3) }
    | ID LPAREN actuals_opt RPAREN { Call($1,   $3) }
    | ID pathattributes            { Pathattr($1, $2) }

pathattributes:
    | PATHNAME                     { Pathname }
    | PATHCREATED                  { Pathcreated }
    | PATHKIND                     { Pathkind }

list_items:
      expr                         { Item($1) }
    | list_items COMMA list_items  { Seq($1, Comma, $3) }           
    
actuals_opt:
    /* nothing */   { [] }
    | actuals_list  { List.rev $1 }

actuals_list:
    expr                      { [$1] }
    | actuals_list COMMA expr { $3 :: $1 }

