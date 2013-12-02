type op = Add | Sub | Mult | Div | In | Equal | Neq | Less | Leq | Greater | Geq

type sep = Comma

type data_type = PathType | StrType | IntType | BoolType | VoidType | DictType | ListType

type pathattr_type = Pathname | Pathcreated | Pathkind

(* mutually recursive types *)
type items = 
    Item of expr
  | Seq of items * sep * items
and expr =
    LitInt of int
  | LitStr of string
  | LitBool of bool
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Copy of string * expr
  | Move of string * expr
  | List of items
  | Pathattr of string * pathattr_type
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt 
  | For of string * expr * stmt
  | While of expr * stmt
  | Print of expr

type var_decl = {
  vtype : data_type;
  vname : string;
}

type func_decl = {
    return : data_type;
    fname : string;
    formals : var_decl list;
    fnlocals : var_decl list;
    body : stmt list;
  }


type program = var_decl list * func_decl list
