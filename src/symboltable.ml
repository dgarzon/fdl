open Ast

module StringMap = Map.Make(String)

type env = {
    locals:         string StringMap.t;
	globals:        string StringMap.t;
	functions:      string list StringMap.t;
}

let string_of_vtype = function
  VoidType -> "void"
  | IntType -> "int"
(*<<<<<<< HEAD*)
  | StrType -> "char *"
  | BoolType -> "int"
  | PathType -> "struct Path *"
  | DictType -> "struct Dictionary *"
  | ListType -> "struct List *"
(*=======*)
  | StrType -> "string"
  | BoolType -> "boolean"
  | PathType -> "path"
  | DictType -> "dict"
  | ListType -> "list"
(*>>>>>>> caec229e1895298c4c1cf4ea51802cce238e2e64*)

let find_variable name env =
	try StringMap.find name env.locals
	with Not_found -> try StringMap.find name env.globals
	with Not_found -> ""
	(*raise (Failure ("undefined variable " ^ name)) *)

let find_function name env =
	try StringMap.find name env.functions
	with Not_found -> []
	(*raise (Failure ("undefined function " ^ name)) *)

let add_local name v_type env =
	if StringMap.mem name env.locals then StringMap.empty
	else StringMap.add name (string_of_vtype v_type) env.locals

let add_global name v_type env =
	if StringMap.mem name env.globals then StringMap.empty
	else StringMap.add name (string_of_vtype v_type) env.globals

(* from the ast *)
let get_arg_type = function
	v -> string_of_vtype v.vtype

let add_function name return_type formals env =
	if StringMap.mem name env.functions then StringMap.empty
	else let f = List.map get_arg_type formals in
	StringMap.add name (string_of_vtype (return_type)::f) env.functions
