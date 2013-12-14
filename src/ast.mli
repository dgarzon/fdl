type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type sep = Comma

type data_type = PathType | StrType | IntType | BoolType | VoidType | ListType

type pathattr_type = Pathname | Pathcreated | Pathkind | Pathext

type list_expr = 
    ListId of string 
  | ListItemInt of int
  | ListItemStr of string

type items = 
    Item of expr
  | Seq of expr * sep * items
  | Noitem
and expr =
    LitInt of int
  | LitStr of string
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Copy of expr * expr
  | Move of expr * expr
  | List of items
  | ListAppend of string * list_expr
  | ListRemove of string * list_expr
  | Pathattr of string * pathattr_type
  | Noexpr


type for_expr = 
    Forid of string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt 
  | For of for_expr * for_expr * stmt
  (* | For of expr * expr * stmt*)
  | While of expr * stmt
  | Print of expr
  | Ifin of list_expr * list_expr * stmt * stmt

type var_decl = {
  vtype : data_type;
  vname : string;
  vexpr : expr;
}

type func_decl = {
    return : data_type;
    fname : string;
    formals : var_decl list;
    fnlocals : var_decl list;
    body : stmt list;
  }


type program = var_decl list * func_decl list