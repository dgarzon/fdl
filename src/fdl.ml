open Ast

let rec string_of_expr = function
    LitInt(l) -> string_of_int l
  | LitStr(l) -> l
  | Id(s) -> s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ");"


let rec string_of_stmt = function
  Expr(expr) -> string_of_expr expr ^ "\n"

let string_of_vtype = function
  VoidType -> "void"
  | IntType -> "int"
  | StrType -> "char *"
  | BoolType -> "int"
  | PathType -> "char *"
(* Need to match dict and list *)

let string_of_vdecl vdecl = string_of_vtype vdecl.vtype ^ " " ^ vdecl.vname ^ ";\n"

let string_of_fdecl fdecl =
  string_of_vtype fdecl.return ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_fdecl funcs)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let listing = string_of_program program in
  print_string listing
