{ open Parser }

let letter = ['a' - 'z' 'A' - 'Z']
let digit = ['0' - '9']
let quote = '"'

rule token = parse
	[' ' '\r' '\n']	{ token lexbuf }	| "/*"		{ comment lexbuf }
	| '\t' 			{ TAB }
	| '('			{ LPAREN }		| ')'			{ RPAREN }
	| '{'			{ LBRACE }		| '}'			{ RBRACE }
	| ';'			{ SEMI }		| ','			{ COMMA }
	| '+'			{ PLUS }		| '-'			{ MINUS }
	| '*'			{ TIMES }		| '/'			{ DIVIDE }
	| '='			{ ASSIGN }
	| "<<-"			{ MOVE }
	| "<-"			{ COPY }
	| "=="			{ EQ }			| "!="			{ NEQ }
	| '<'			{ LT }			| "<="			{ LEQ }
	| '>'			{ GT }			| ">="			{ GEQ }
	| '['			{ LBRACK }		| ']'			{ RBRACK }
	| "&&"			{ AND }			| "||"			{ OR }
	| '!'			{ NOT }
	| "def"			{ DEF }
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
	| "trash"		{ TRASH }
	| eof			{ EOF }			(* do as microC *)
	| digit+ as lit					{ LIT_INT(int_of_string lit) }
	| quote [^'"']* quote as lit	{ LIT_STR(lit) }
	| letter | (letter | digit | '_')* as id		{ ID(id) }
	| _ as char 		{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
	"//"			{ token lexbuf }
	| _				{ comment lexbuf}