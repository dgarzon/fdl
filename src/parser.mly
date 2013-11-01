%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN MOVE COPY
%token EQ NEQ LT LEQ GT GEQ LESS GTR
%token AND OR
%token CONTINUE BREAK
%token DEF RETURN IF THEN ELSE FOR IN WHILE INT STR
%token DICT LIST PATH TRASH MAIN
%token <int> LIT_INT
%token <string> LIT_STR
%token <string> ID
%token EOF

%nonassoc ELSE
%nonassoc DO
%nonassoc THEN

%right ASSIGN

%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.main> program

%%

program:
   /* nothing */ { [], [] }
   | program vdecl { ($2 :: fst $1), snd $1 }
   | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
    INT ID LPAREN formals_opt RPAREN stmt_list
        {{
            return = literal_int;
            function_name = $2;
            parameters = $4;
            body = $6;
        }}
    | STR ID LPAREN formals_opt RPAREN stmt_list
        {{
            return = literal_str;
            function_name = $2;
            parameters = $4;
            body = List.rev $6;
        }}
    | PATH ID LPAREN formals_opt RPAREN stmt_list
        {{
            return = path_type;
            function_name = $2;
            parameters = $4;
            body = List.rev $6;
        }}
    | DICT ID LPAREN formals_opt RPAREN stmt_list
        {{
            return = dict_type;
            function_name = $2;
            parameters = $4;
            body = List.rev $6;
        }}
    | LIST ID LPAREN formals_opt RPAREN stmt_list
        {{
            return = list_type;
            function_name = $2;
            parameters = $4;
            body = List.rev $6;
        }}

formals_opt:
    { [] }
    | formals_list   { List.rev $1 }

formals_list:
    formal                        { [$1] }
    | formals_list COMMA formal { $3 :: $1 }

formal:
    INT ID {{var_type = literal_int; var_name = $2}}
    | STR ID {{var_type = literal_str; var_name = $2}}
    | PATH ID {{var_type = path_type; var_name = $2}}
    | DICT ID {{var_type = dict_type; var_name = $2}}
    | LIST ID {{var_type = list_type; var_name = $2}}

vdecl:
    INT ID {{var_type = Ast.Int; var_name = $2;  data_type = Ast.Int}}
    | STR ID {{var_type = Ast.Str; var_name = $2; data_type = Ast.Str}}
    | PATH ID {{var_type = Ast.Path; var_name = $2; data_type = Ast.Path}}
    | DICT ID {{var_type = Ast.Dict; var_name = $2; data_type = Ast.Dict}}
    | LIST ID {{var_type = Ast.List; var_name = $2; data_type = Ast.List}}

stmt_list:
    { [] }
    | stmt_list stmt { $2 :: $1 }

stmt:
    expr { Expr($1) }
    | RETURN expr { Return($2) }
    | stmt_list { Block(List.rev $2) }
    | IF LPAREN expr RPAREN THEN stmt { If($3, $6, Block([])) }
    | IF LPAREN expr RPAREN THEN stmt ELSE stmt    { If($3, $6, $8) }

expr: