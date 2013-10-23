{ open Parser }

let letter = ['a' - 'z' 'A' - 'Z']
let digit = ['0' - '9']
let quote = '"'

rule token = parse
	[' ' '\t' '\r' '\n']	{ token lexbuf }	| "/*"		{ comment lexbuf }
	| '('			{ LPAREN }		| ')'			{ RPAREN }
	| '{'			{ LBRACE }		| '}'			{ RBRACE }
	| ';'			{ SEMI }		| ','			{ COMMA }
	| '+'			{ PLUS }		| '-'			{ MINUS }
	| '*'			{ TIMES }		| '/'			{ DIVIDE }
	| '='			{ ASSIGN }		
	| "=="			{ EQ }			| "!="			{ NEQ }
	| '<'			{ LESS }		| "<="			{ LEQ }
	| '>'			{ GRT }			| ">="			{ GEQ }
	| '['			{ LBRACK }		| ']'			{ RBRACK }
	| "&&"			{ AND }			| "||"			{ OR }
	| '!'			{ NOT }			
	| "int"			{ INT }			| "path"		{ PATH }
	| "string"		{ STR }			| "bool"		{ BOOL }
	| "if"			{ IF }			| "else"		{ ELSE }
	| "while"		{ WHILE }		| "return"		{ RETURN }
	| "break"		{ BREAK }		| "continue"	{ BREAK }
	| "void"		{ VOID }		
	| "true"		{ TRUE }		| "false"	{ FALSE }
	| eof			{ EOF }			(* do as microC *)
	| digit+ as lit					{ LIT_INT(int_of_string lit) }
	| quote [^'"']* quote as lit	{ LIT_STR(lit) }
	| letter | '_' (letter | digit | '_')* as id		{ ID(id) }
	| _ as char 		{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
	"*/"			{ token lexbuf }
	| _				{ comment lexbuf}