%{ open ast %}

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
    { [], [] }
    | program vdecl { ($2 :: fst $1), snd $1 }
    | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
    DEF VOID ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
       {{
        fname = $3;
        formals = $5;
        locals = List.rev $8;
        body = List.rev $9 }}
   | DEF INT ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      {{
        return = IntType;
        fname = $3;
        formals = $5;
        locals = List.rev $8;
        body = List.rev $9 }}
   | DEF STR ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      {{
        return = StrType;
        fname = $3;
        formals = $5;
        locals = List.rev $8;
        body = List.rev $9 }}
   | DEF PATH ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      {{
        return = PathType;
        fname = $3;
        formals = $5;
        locals = List.rev $8;
        body = List.rev $9 }}
   | DEF BOOL ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      {{
        return = BoolType;
        fname = $3;
        formals = $5;
        locals = List.rev $8;
        body = List.rev $9 }}

formals_opt:
    { [] }
    | formal_list   {{ List.rev $1 }}

formal_list:
    formal                   {{ [$1] }}
    | formal_list COMMA formal {{ $3 :: $1 }}

formal:
    INT ID      {{ VarType = IntType; name = $2; }}
    | BOOL ID     {{ VarType = BoolType; name = $2; }}
    | PATH ID     {{ VarType = PathType; name = $2; }}
    | STR ID      {{ VarType = StrType; name = $2; }}
    | DICT ID     {{ VarType = DictType; name = $2; }}
    | LIST ID     {{ VarType = ListType; name = $2; }}


vdecl_list:
    { [] }
    | vdecl_list vdecl { $2 :: $1 }

vdecl:
    INT ID    {{VarType = IntType;  VarName = $2; }}
    | BOOL ID   {{VarType = BoolType; VarName = $2; }}
    | STR ID    {{VarType = StrType;  VarName = $2; }}
    | PATH ID   {{VarType = PathType; VarName = $2; }}
    | DICT ID   {{VarType = DictType; VarName = $2; }}
    | LIST ID   {{VarType = ListType; VarName = $2; }}

stmt_list:
    { [] }
    | stmt_list stmt { $2 :: $1 }

stmt:
    expr { Expr($1) }
    | RETURN expr                                  { Return($2) }
    | IF LPAREN expr RPAREN THEN stmt %prec NOELSE { If($3, $6, Block([])) }
    | IF LPAREN expr RPAREN THEN stmt ELSE stmt    { If($3, $6, $8) }

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

