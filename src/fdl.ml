(* open Ast *)
open Sast

let rec create_node_string e = match e
    with
      LitInt(l) -> "createIntNode("^string_of_int l^",fdl_int)"
    | LitStr(l) -> "createStrNode("^l^",fdl_str)"
    | _ -> ""

and string_of_items = function
    Item(e) -> "addBack(&temp_list," ^ create_node_string e ^ ");\n"

  | Seq(e, sep, i2) -> "addBack(&temp_list," ^ create_node_string e ^ ");\n"
                    ^ (string_of_items i2)
  | Noitem -> ""

and string_of_expr = function
    LitInt(l) -> string_of_int l
  | LitBool(l) -> string_of_bool l
  | LitStr(l) -> l
  | Id(s) -> s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ");"
  | Binop(e1, o, e2) ->
      if o = Sast.StrEqual then
        "!strcmp(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
        (* strcmp returns 0 for match, that's why the ! *)
      else if o = Sast.StrNeq then
        "strcmp(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
        (* strcmp returns 0 for match *)
      else
      string_of_expr e1 ^ " " ^
      ( match o with
          Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | In -> "in"
        | Equal -> "==" | Neq -> "!="
        | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" 
        | And -> "&&" | Or -> "||" ) ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  (* Maybe used built-in functions for copy and move *)
   (* | Copy(e_dest, e_src) -> "execl(\"/bin/cp\",\"/bin/cp\"," ^ string_of_expr e_src ^ "," ^ string_of_expr e_dest ^ ", (char *) 0)" *)
   | Copy(e_dest, e_src) -> "copyFile(" ^ string_of_expr e_src ^ ", " ^ string_of_expr e_dest ^ ")"
(*        "execl("/bin/cp", "/bin/cp"," ^  string_of_expr e ^ "," ^ string_of_expr v ^ ", (char star) 0)" *)
(* --must deal with quotes in expression definition, replace 'star' with actual symbol    *)
  | Move(e_dest, e_src) -> "moveFile(" ^ string_of_expr e_src ^ ", " ^ string_of_expr e_dest ^ ")"
  | List(i) -> "&temp_list;\ninitList(&temp_list);\n" ^ string_of_items i
  | ListAppend(id, e) -> let arg = (match e with
                          ListItemInt(l) -> "createIntNode("^string_of_int l^",fdl_int)"
                        | ListItemStr(l) -> "createStrNode("^l^",fdl_str)"
                        | ListId(i, t) -> if t = "path" || t = "string" then
                                        "createStrNode(strdup("^i^"),fdl_str)"
                                      else if t = "int" || t = "bool" then
                                        "createIntNode("^i^",fdl_int)"
                                      else raise (Failure ("Invalid id type used in If-in statement."))
                      ) in
                        "addBack("^ id ^ ", " ^ arg ^ ");\n"
  | ListRemove(id, e) -> let arg = (match e with
                          ListItemInt(l) -> "createIntNode("^string_of_int l^",fdl_int)"
                        | ListItemStr(l) -> "createStrNode("^l^",fdl_str)"
                        | ListId(i, t) -> if t = "path" || t = "string" then
                                        "createStrNode("^i^",fdl_str)"
                                      else if t = "int" || t = "bool" then
                                        "createIntNode("^i^",fdl_int)"
                                      else raise (Failure ("Invalid id type used in If-in statement."))
                      ) in
                        "removeNode("^ id ^ ", " ^ arg ^ ");\n"
  | Pathattr(id, e) -> ( match e with
                          Pathname -> "getPathName(" ^ id ^ ")"
                          | Pathcreated -> "getCreatedAt(" ^ id ^ ")"
                          | Pathkind -> "getPathType(" ^ id ^ ")"
                          | Pathext -> "getExtension(" ^ id ^ ")"
                        )
  | Noexpr -> ""

let get_list_arg le = match le
  with
  ListId(i, t) -> i
  | _ -> raise (Failure ("If-in second argument must be of type list."))

let get_for_id e = match e
    with
    Forid(id) -> id
  | _ -> raise (Failure("For loops work only with type path!"))

let rec string_of_stmt = function
    Expr(expr) -> if compare (string_of_expr expr) "" = 0 then "\n" else string_of_expr expr ^ ";\n"
  | Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n}"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
      (* print needs to be made aware of expr type, otherwise won't work *)
  | Print(expr, expr_type) -> if expr_type = "string" || expr_type = "path" then
                                "printf(\"%s\\n\"," ^ string_of_expr expr ^ ");\n"
                              else if expr_type = "int" || expr_type = "bool" then
                                "printf(\"%d\\n\"," ^ string_of_expr expr ^ ");\n"
                              else 
                                "traverseList(" ^ string_of_expr expr ^",&printNode);\n"
                              

  (*| For(e1, e2, s1) ->  "for (" ^ string_of_expr e1 ^ " in "
      ^ string_of_expr e2 ^ ")\n" ^ string_of_stmt s1*)
  | For(e1, e2, s1) ->  (*"while (" ^ (get_list_arg le2) ^")\n if(findNode(" ^ (get_list_arg le2) ^","^arg^") == 0)\n"^string_of_stmt s1*)
      "initList(&subPathList);\n" ^
      "loadDirectoryToList(" ^ get_for_id e2 ^ ", &subPathList);\n" ^
      "node = subPathList.head;\n"^
      (*"char *"^get_for_id e1^";\n"^*)
      "while(node){\n"^
      get_for_id e1 ^" = node->string_item;\n"^
      string_of_stmt s1 ^
      "node = node->next;\n"^
      "}\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | Ifin(le1,le2,s1,s2) -> let arg = (match le1 with
                                    ListItemInt(l) -> "createIntNode("^string_of_int l^",fdl_int)"
                                  | ListItemStr(l) -> "createStrNode("^l^",fdl_str)"
                                  | ListId(i, t) -> if t = "path" || t = "string" then
                                        "createStrNode("^i^",fdl_str)"
                                      else if t = "int" || t = "bool" then
                                        "createIntNode("^i^",fdl_int)"
                                      else raise (Failure ("Invalid id type used in If-in statement."))
                                  ) in
                                  "if(findNode(" ^ (get_list_arg le2) ^","^arg^") == 0)\n"^
                                string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2

let string_of_vtype = function
  VoidType -> "void"
  | IntType -> "int"
  | StrType -> "char *"
  | BoolType -> "int"
  | PathType -> "char *"
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
  "\n#include<stdio.h>\n#include<stdlib.h>\n#include<string.h>\n#include \"list.h\"\n" ^
  "#include \"path.h\"\n struct List temp_list;\nstruct Node *node;\n" ^ "struct List subPathList;\n" ^
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
      let suffix = String.sub fname index 5 in
      if not (suffix = ".fdlp") then raise (Failure ("Invalid type of source file."))
      else
        (* lex from the file *)
        let input = open_in fname in
        let lexbuf = Lexing.from_channel input in
        let program = Parser.program Scanner.token lexbuf in
        (* added the type check *)
        let program_t = Typecheck.check_program program in
        let listing = string_of_program program_t in
        print_string listing

