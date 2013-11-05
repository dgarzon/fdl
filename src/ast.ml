type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

(* data_type can be matched with C types *)
type data_type = PathType | StrType | IntType | BoolType | VoidType | DictType | ListType

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

(* New var_decl type to bind data_type with IDs *)
type var_decl = {
  vtype : data_type;
  vname : string;
}


type func_decl = {
    return : data_type;
    fname : string;
    formals : var_decl list;
    locals : var_decl list;
    body : stmt list;
  }


type program = string list * func_decl list

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

