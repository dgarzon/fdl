type op_t = Add | Sub | Mult | Div | In | Equal | Neq | Less | Leq | Greater | Geq

type sep_t = Comma

type data_type_t = PathType | StrType | IntType | BoolType | VoidType | DictType | ListType

type pathattr_type_t = Pathname | Pathcreated | Pathkind

type list_expr_t = 
    ListId of string * string
  | ListItemInt of int
  | ListItemStr of string
  | ListItemBool of bool

type items_t = 
    Item of expr_t
  | Seq of expr_t * sep_t * items_t
and expr_t =
    LitInt of int
  | LitStr of string
  | LitBool of bool
  | Id of string
  | Binop of expr_t * op_t * expr_t
  | Assign of string * expr_t
  | Call of string * expr_t list
  | Copy of expr_t * expr_t
  | Move of expr_t * expr_t
  | List of items_t
  | ListAppend of string * list_expr_t
  | ListRemove of string * list_expr_t
  | Pathattr of string * pathattr_type_t
  | Noexpr

type for_expr_t = 
    Forid of string
  
type stmt_t =
    Block of stmt_t list
  | Expr of expr_t
  | Return of expr_t
  | If of expr_t * stmt_t * stmt_t 
  (*| For of expr_t * expr_t * stmt_t *)
  | For of for_expr_t * for_expr_t * stmt_t
  | While of expr_t * stmt_t
  | Print of expr_t * string
  | Ifin of list_expr_t * list_expr_t * stmt_t * stmt_t
  (* the string in Print represents its type, need to change to data_type_t *)

type var_decl_t = {
  vtype : data_type_t;
  vname : string;
}

type func_decl_t = {
    return : data_type_t;
    fname : string;
    formals : var_decl_t list;
    fnlocals : var_decl_t list;
    body : stmt_t list;
  }

type program_t = var_decl_t list * func_decl_t list
