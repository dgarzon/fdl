type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    LitInt of int
  | LitStr of string
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    return : string;
    fname : string;
    formals : var_decl list;
    locals : string list;
    body : stmt list;
  }

type var_decl = {
  vtype : string;
  vname : string;
}

type program = string list * func_decl list

let rec string_of_stmt = function
  Expr(expr) -> string_of_expr expr ^ ";\n";

let string_of_vdecl vtype id = vtype ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.return ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat (List.map string_of_fdecl funcs)