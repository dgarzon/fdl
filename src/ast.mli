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
  | Copy of string * expr
  | Move of string * expr
  | Noexpr
(* Should copy/move be string * string or string * expr? *)
(* I think it should be string * string -Cara *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of string * expr * stmt
  | While of expr * stmt
  | Print of expr

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

(* should fname be a var_decl as opposed to a string? where do we specify the return type of the function? -Cara *)

type program = var_decl list * func_decl list
