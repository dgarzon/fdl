open Ast

let rec string_of_expr = function
    LitInt(l) -> string_of_int l
  | LitStr(l) -> l
  | Id(s) -> s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ");"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      ( match o with
          Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
        | Equal -> "==" | Neq -> "!="
        | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      )
      ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  (* Maybe used built-in functions for copy and move *)
  | Copy(v, e) -> v ^ " = " ^ string_of_expr e
  | Move(v, e) -> v ^ " = " ^ string_of_expr e
  | Noexpr -> ""


let rec string_of_stmt = function
    Expr(expr) -> string_of_expr expr ^ "\n"
  | Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
      (* print needs to be made aware of expr type, otherwise won't work *)
  | Print(expr) -> "printf(" ^ string_of_expr expr ^ ");\n"
  | For(e1, e2, e3, s1) ->  "for (" ^ string_of_expr e1 ^ "; "
      ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ")\n" ^ string_of_stmt s1
  | While(e, s) -> "while (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s

let string_of_vtype = function
  VoidType -> "void"
  | IntType -> "int"
  | StrType -> "char *"
  | BoolType -> "int"
  | PathType -> "char *"
  | DictType -> "struct Dictionary *"
  | ListType -> "struct List *"

(* variable declrarations, has ;*)
let string_of_vdecl vdecl = string_of_vtype vdecl.vtype ^ " " ^ vdecl.vname ^ ";\n"

(* formal argument declrarations, has no ;*)
let string_of_formaldecl vdecl = string_of_vtype vdecl.vtype ^ " " ^ vdecl.vname

let string_of_fdecl fdecl =
  string_of_vtype fdecl.return ^ " " ^ fdecl.fname ^ "(" ^
    String.concat ", " (List.map string_of_formaldecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.fnlocals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "\n" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

let _ =
  (* let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let listing = string_of_program program in
  print_string listing *)

  (* first argument is the filename *)
  let fname = Sys.argv.(1) in 
      (* check the extension *)
      let index = (if String.contains fname '.' then String.rindex fname '.' else 0 ) in 
      let suffix = String.sub fname index 4 in
      if not (suffix = ".fdl") then raise (Failure ("Invalid type of source file.")) 
      else 
        (* lex from the file *)
        let input = open_in fname in
        let lexbuf = Lexing.from_channel input in
        let program = Parser.program Scanner.token lexbuf in
        (* added the type check *)
        let program_t = Typecheck.check_program program in
        let listing = string_of_program program_t in
        print_string listing
  
