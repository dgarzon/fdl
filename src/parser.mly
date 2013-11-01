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
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }