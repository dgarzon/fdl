{ open Parser }

let letter = ['a' - 'z' 'A' - 'Z']
let digit = ['0' - '9']
let quote = '"'

rule token = parse
	[' ' '\r' '\n' '\t']	{ token lexbuf }	| "/*"		{ comment lexbuf }
	| '('			{ LPAREN }		| ')'			{ RPAREN }
	| '{'			{ LBRACE }		| '}'			{ RBRACE }		| ','			{ COMMA }
	| '+'			{ PLUS }		| '-'			{ MINUS }
	| '*'			{ TIMES }		| '/'			{ DIVIDE }
	| '='			{ ASSIGN }		| ';'			{ SEMI }
	| "<<-"			{ MOVE }
	| "<-"			{ COPY }
	| "=="			{ EQ }			| "!="			{ NEQ }
	| '<'			{ LT }			| "<="			{ LEQ }
	| '>'			{ GT }			| ">="			{ GEQ }
	| '['			{ LBRACK }		| ']'			{ RBRACK }
	| "&&"			{ AND }			| "||"			{ OR }
	| '!'			{ NOT }
	| "def"			{ DEF }			| "print"		{ PRINT }
	| "int"			{ INT }			| "path"		{ PATH }
	| "dict"		{ DICT }		| "list"		{ LIST }
	| "string"		{ STR }			| "bool"		{ BOOL }
	| "if"			{ IF }			| "else"		{ ELSE }
	| "then"		{ THEN }
	| "for"			{ FOR }			| "in"			{ IN }
	| "do"			{ DO }
	| "while"		{ WHILE }		| "return"		{ RETURN }
	| "break"		{ BREAK }		| "continue"	{ CONTINUE }
	| "void"		{ VOID }
	| "true"		{ TRUE }		| "false"	{ FALSE }
	(* Do we need true and false? *)
	| "trash"		{ TRASH }
	| eof			{ EOF }			(* do as microC *)
	| digit+ as lit					{ LIT_INT(int_of_string lit) }
	| quote [^'"']* quote as lit	{ LIT_STR(lit) }
	| letter | (letter | digit | '_')* as id		{ ID(id) }
	| _ as char 		{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
	"*/"			{ token lexbuf }
	| _				{ comment lexbuf}