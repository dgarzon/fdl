open Ast
open Symboltable
(*adding testing comment *)
module StringMap = Map.Make(String)

let string_of_vtype = function
  VoidType -> "void"
  | IntType -> "int"
  | StrType -> "string"
  | BoolType -> "bool"
  | PathType -> "path"
  | ListType -> "list"

let get_sast_type = function
	Ast.PathType -> Sast.PathType
	| Ast.StrType -> Sast.StrType
	| Ast.IntType -> Sast.IntType
	| Ast.BoolType -> Sast.BoolType
	| Ast.VoidType -> Sast.VoidType
	| Ast.ListType -> Sast.ListType

let get_sast_pathattrtype = function
	Ast.Pathname -> Sast.Pathname, "string"
	| Ast.Pathcreated -> Sast.Pathcreated, "int"
	| Ast.Pathkind -> Sast.Pathkind, "int"
	| Ast.Pathext -> Sast.Pathext, "string"



(* get variable type according to the name
 * raise error if no name matching in variable list *)
let get_vtype env id = 
	(* find_variable method is from the symbol table *)
	let t = find_variable id env in
	if t = "" then raise (Failure ("undefined variable: " ^ id)) else t

(* get the type of expression:
 *  -> string if one of the two operands having string type
 *  -> int/boolean if both of the operands having the same type *)
let get_expr_type t1 t2 =
	if t1 = "void" || t2 = "void" then raise (Failure ("cannot use void type inside expression")) else
	if t1 = "string" || t2 = "string" then "string" else
	if t1 = "int" && t2 = "int" then "int" else
	if t1 = "bool" && t2 = "bool" then "bool" else
	if t1 = "int" && t2 = "bool" then "int" else
	if t1 = "bool" && t2 = "int" then "int" else
	raise (Failure ("type error"))


let check_listexpr env = function
	| Ast.ListId(id) ->
		Sast.ListId(id, get_vtype env id), get_vtype env id
	| Ast.ListItemInt(i) -> Sast.ListItemInt(i), "int"
	| Ast.ListItemStr(s) -> Sast.ListItemStr(s), "string"
(* mark int & boolean expression to string type *)
(* let conv_type = function 
	(expr, t) -> if t = "void" then raise (Failure ("cannot use void type inside expression")) else
			(* Need to add a ToStr rule in SAST *)
		     if not(t = "string") then Ast.ToStr(expr) else expr *)

(* check the expression type can be used for
 * the corresponding argument according to definition
 * return the new expression list in expr_t for sast *)
let check_func_arg lst expr arg_t =
	if (snd expr) = arg_t then (fst expr)::lst else
	raise (Failure("function arguments do not match"))

let match_oper e1 op e2 =
	(* snd of expr is type *)
	let expr_t = get_expr_type (snd e1) (snd e2) in
	(match op with
	   Ast.Add -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Add, fst e2), "int") else
	   			if expr_t = "string" then (Sast.Binop(fst e1, Sast.StrAdd, fst e2), "string") else
		  		raise (Failure ("type error"))
	 | Ast.Sub -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Sub, fst e2), "int") else
		  raise (Failure ("type error"))
	 | Ast.Mult -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Mult, fst e2), "int") else
	 	   raise (Failure ("type error"))
	 | Ast.Div -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Div, fst e2), "int") else
		  raise (Failure ("type error"))
		  (* equal and not equal have special case for string comparison 
		  		we may need to add SAST and Eqs and Neqs *)
	 | Ast.Equal -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Equal, fst e2), "bool") else
	 				if expr_t = "string" then (Sast.Binop(fst e1, Sast.StrEqual, fst e2), "bool") else
                  raise (Failure ("type error in == "))
	 | Ast.Neq -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Neq, fst e2), "bool") else
	 				if expr_t = "string" then (Sast.Binop(fst e1, Sast.StrNeq, fst e2), "bool") else
                  raise (Failure ("type error"))
	 | Ast.Less ->if expr_t = "int" then (Sast.Binop(fst e1, Sast.Less, fst e2), "bool") else
                  raise (Failure ("type error")) 
	 | Ast.Leq ->if expr_t = "int" then (Sast.Binop(fst e1, Sast.Leq, fst e2), "bool") else
                  raise (Failure ("type error"))
	 | Ast.Greater ->if expr_t = "int" then (Sast.Binop(fst e1, Sast.Greater, fst e2), "bool") else
                  raise (Failure ("type error"))
	 | Ast.Geq ->if expr_t = "int" then (Sast.Binop(fst e1, Sast.Geq, fst e2), "bool") else
                  raise (Failure ("type error")) 
     | Ast.And ->if expr_t = "bool" then (Sast.Binop(fst e1, Sast.And, fst e2), "bool") else
      			  raise (Failure ("type error in and")) 
     | Ast.Or ->if expr_t = "bool" then (Sast.Binop(fst e1, Sast.Or, fst e2), "bool") else
      			  raise (Failure ("type error in or")) 
	)

(* it returns the expr and its type *)
let rec check_expr env = function
	Ast.LitInt(i) -> Sast.LitInt(i), "int"
	| Ast.LitStr(s) -> Sast.LitStr(s), "string"

	| Ast.Id(id) ->
		Sast.Id(id), (get_vtype env id)

	| Ast.Binop(e1, op, e2) ->
		match_oper (check_expr env e1) op (check_expr env e2)

	| Ast.Assign(id, e) ->
		let t = get_vtype env id in
		(* if t = "string" then 
		     Ast.AssignStr(id, (conv_type (check_expr env e))), "void"
		else  *)
		Sast.Assign(id, (get_expr_with_type env e t)), "void"
	
	| Ast.Call(func, el) ->
		(* find_function is from the symbol table *)
		let args = find_function func env in	(* return & arguments type list from definition *)
		( match args with
			[] -> raise (Failure ("undefined function " ^ func))
			| hd::tl -> let new_list = try List.fold_left2 check_func_arg [] (List.map (check_expr env) el) tl
						   with Invalid_argument "arg" -> raise(Failure("unmatched argument list"))
				    in Sast.Call(func, List.rev new_list ), hd )
		(* Need to add type checking for Move and Copy *)
  	| Ast.Move(e1, e2) -> 
		let e_t1 = check_expr env e1 in
		let e_t2 = check_expr env e2 in
		if snd e_t1 = "path" && snd e_t2 = "path"
			then Sast.Move(fst e_t1, fst e_t2), "void"
		else 
			raise(Failure("cannot use path function on non-path variables"))
  	| Ast.Copy(e1, e2) -> 
	 	let e_t1 = check_expr env e1 in
                let e_t2 = check_expr env e2 in
                if snd e_t1 = "path" && snd e_t2 = "path"
                        then Sast.Copy(fst e_t1, fst e_t2), "void"
                else
                        raise(Failure("cannot use path function on non-path variables"))
	| Ast.List(items) -> Sast.List(check_list_items env items), "list"
	| Ast.ListAppend(id, item) -> let t1 = get_vtype env id in
								  let t2 = check_listexpr env item in
								if not (t1 = "list") 
									then raise(Failure("Can append only to id of type list."))
								else if ((snd t2) = "list")
									then raise(Failure("Cannot append list to list."))
								else
									Sast.ListAppend( id, (fst t2)), "void"
	| Ast.ListRemove(id, item) -> let t1 = get_vtype env id in
								  let t2 = check_listexpr env item in
								if not (t1 = "list") 
									then raise(Failure("Can call remove only on type list."))
								else if ((snd t2) = "list")
									then raise(Failure("Cannot remove a list from list."))
								else
									Sast.ListRemove(id, (fst t2)), "void"
	| Ast.Pathattr(id, e) ->
		if not ((get_vtype env id) = "path")
			then raise(Failure("cannot use path attributes on non-path variable " ^ id))
		else
		(* return type is string assuming path attributes will be treated that way *)
			Sast.Pathattr(id, fst (get_sast_pathattrtype e)), snd (get_sast_pathattrtype e)
	| Ast.Noexpr -> Sast.Noexpr, "void"

and check_list_items env = function
	  Ast.Item(e) ->let i,t = check_expr env e in 
	  				Sast.Item(i)
	| Ast.Seq(e1, sep, e2) -> Sast.Seq(fst (check_expr env e1), Sast.Comma, (check_list_items env e2))
	| Ast.Noitem -> Sast.Noitem

(* get expr_t(sast type) by expr(ast type) with given type
 * raise error if the expression type does match requirement, snd e has the type and fst has the expr *)
and get_expr_with_type env expr t = 
	let e = check_expr env expr in
	(* added special case for the path variable *)
	if ((snd e) = "string" && t = "path") then (fst e)
	else if ((snd e) = "int" && t = "bool") then (fst e)
	else if not((snd e) = t) then raise (Failure ("type error")) else (fst e)


let check_forexpr env = function
	Ast.Forid(id) -> Sast.Forid(id), get_vtype env id
	
let rec check_stmt env func = function
	  Ast.Block(stmt_list) -> (Sast.Block(check_stmt_list env func stmt_list)), env
	(* | Decl(s1, s2, expr) -> let e = check_expr env expr in
				(*modified: 1. check s1 cannot be void; 2. expr can be void*)
							if s1 = "void" then raise (Failure("cannot use void as variable type")) else
							if not(snd e = s1) && not(snd e = "void") && not(s1 = "string") then raise (Failure ("type error"))
	        				else let ret = add_local s2 s1 env in 
	        				if StringMap.is_empty ret then raise (Failure ("local variable " ^ s2 ^ " is already defined")) 
	        				else let env = {locals = ret; globals = env.globals; functions = env.functions } in
	        				if s1 = "string" && (snd e = "int" || snd e = "boolean") then (Sast.Decl(s1, s2, Sast.ToStr(fst e))), env 
							else (Sast.Decl(s1, s2, fst e)), env *)
	(*| Ast.VarDecl(vardecl) -> let l,e = check_local env vardecl in
								(Sast.VarDecl(l)),e *)
	(* will pick only fst of the expr, since need onlt the expr and not the type *)
	| Ast.Expr(expr) -> (Sast.Expr(fst (check_expr env expr))), env
	| Ast.Return(expr) -> let e = check_expr env expr in
					  if not(snd e = string_of_vtype func.return) then raise (Failure ("The return type doesn't match!"))
					  else (Sast.Return(fst e)), env 
	| Ast.If(expr, stmt1, stmt2) ->	let e = check_expr env expr in
								if not(snd e = "bool") then raise (Failure ("The type of the condition in If statement must be boolean!")) 
								else (Sast.If(fst e, fst (check_stmt env func stmt1), fst (check_stmt env func stmt2))), env	(* if() {} else{} *)
	| Ast.Ifin(lexpr1, lexpr2, stmt1, stmt2) -> let e1 = check_listexpr env lexpr1 in
								if (snd e1 = "list") then raise (Failure ("Cannot have list in list!")) 
							else let e2 = check_listexpr env lexpr2 in
							if not(snd e2 = "list") then raise (Failure ("\'in\' operator works with list type expression only!"))
						else (Sast.Ifin(fst e1, fst e2, fst (check_stmt env func stmt1), fst (check_stmt env func stmt2))), env
	| Ast.While(expr, stmt) -> let e = check_expr env expr in
						   if not (snd e = "bool") then raise (Failure ("The type of the condition in While statement must be boolean!"))
						   else (Sast.While(fst e, fst (check_stmt env func stmt))), env				(* while() {} *)
   | Ast.For(expr1, expr2, stmt) -> let e1 = check_forexpr env expr1 in let e2 = check_forexpr env expr2 in
						   if not (snd e1 = "path" && snd e2 = "path" ) then raise (Failure("The type of the expression in a For statement must be path"))
						   else (Sast.For(fst e1, fst e2, fst (check_stmt env func stmt))), env 
	(* break statement to be added to AST *)
	(* | Break -> (Ast.Break), env *)
	(* need to add type checking for For *)
	(* | Ast.For (expr1, expr2, expr3, stmt) -> Sast.For(expr1, expr2, expr3, stmt), env *)
	| Ast.Print(expr) -> let (expr, expr_type) = check_expr env expr in
							(Sast.Print(expr , expr_type)), env


and check_stmt_list env func = function 
	  [] -> []
	| hd::tl -> let s,e = (check_stmt env func hd) in s::(check_stmt_list e func tl)

(* convert a variable to its SAST type *)
let convert_to_sast_type x env = 
	let t = get_vtype env x.vname in
		let s_expr = 
		if not (x.vexpr = Ast.Noexpr) then
			get_expr_with_type env x.vexpr t
		else Sast.Noexpr
		in
	{
		Sast.vtype = get_sast_type x.vtype;
		Sast.vname = x.vname;
		Sast.vexpr = s_expr;
	}

let check_formal env formal = 
	let ret = add_local formal.vname formal.vtype env in
	if (string_of_vtype formal.vtype) = "void" then raise (Failure("cannot use void as variable type")) else
	if StringMap.is_empty ret then raise (Failure ("local variable " ^ formal.vname ^ " is already defined"))
	(* update the env with locals from ret *)
	else let env = {locals = ret; globals = env.globals; functions = env.functions } in
	convert_to_sast_type formal env, env

let rec check_formals env formals = 
	match formals with 
	  [] -> []
	| hd::tl -> let f, e = (check_formal env hd) in (f, e)::(check_formals e tl) 

let check_local env local =
	let ret = add_local local.vname local.vtype env in
	if (string_of_vtype local.vtype) = "void" then raise (Failure("cannot use void as variable type")) else
	if StringMap.is_empty ret then raise (Failure ("local variable " ^ local.vname ^ " is already defined"))
	(* update the env with globals from ret *)
	else let env = {locals = ret; globals = env.globals; functions = env.functions } in
	convert_to_sast_type local env, env

let rec check_locals env locals = 
	match locals with
	  [] -> []
	| hd::tl -> let l, e = (check_local env hd) in (l, e)::(check_locals e tl)

(* this function will return the updated formals and body as per the abstract syntax tree, the return type, name and locals *)
let check_function env func =
	(* if List.length func.body = 0 then raise (Failure ("The last statement must be return statement"))
	else if func.fname = "main" && (List.length func.formals) > 0 
	then raise (Failure ("The main function cannot take any argument"))
	else if  func.fname = "main" && ((func.returnType = "int") || (func.returnType = "boolean"))
	then raise (Failure ("The main function cannot can only has type void"))
	else  *)
	match List.hd (List.rev func.body) with
	Return(_) ->
	  	let env = {locals = StringMap.empty; globals = env.globals; functions = env.functions } in
	  	(*  ret is new env *)
		let ret = add_function func.fname func.return func.formals env in
		if StringMap.is_empty ret then raise (Failure ("function " ^ func.fname ^ " is already defined"))
		(* update the env with functions from ret *)
		else let env = {locals = env.locals; globals = env.globals; functions = ret } in
		(* check the formal arguments, returns formal list appended with their env *)
		let f = check_formals env func.formals in
		(* get the list of formals from f *)
		let formals = List.map (fun formal -> fst formal) f in
		
		(* get the final env from the last formal *)
		let l, env = 
		(match f with
			  [] -> let l = check_locals env func.fnlocals in
					 l, env
			| _ -> 	let env = snd (List.hd (List.rev f)) in
					let l = check_locals env func.fnlocals in
					l, env
		) in
		let fnlocals = List.map (fun fnlocal -> fst fnlocal) l in
		 (match l with
		 	(* empty f, no fomal args *)
	            [] -> let body = check_stmt_list env func func.body in
	                { Sast.return = get_sast_type func.return; 
	                  Sast.fname = func.fname; 
	                  Sast.formals = formals; 
	                  Sast.fnlocals = fnlocals; 
	                  Sast.body = body
	                }, env

	            (* get the final env from the last formal *)
	            | _ -> let e = snd (List.hd (List.rev l)) in
	                   let body = check_stmt_list e func func.body in
	                  { Sast.return = get_sast_type func.return; 
	                    Sast.fname = func.fname; 
	                    Sast.formals = formals; 
	                    Sast.fnlocals = fnlocals; 
	                    Sast.body = body
	                  }, e 
          )

	| _ -> raise (Failure ("The last statement must be return statement"))


let rec check_functions env funcs = 
	match funcs with
	  [] -> []
	| hd::tl -> let f, e = (check_function env hd) in f::(check_functions e tl) 

(* returns the global and its env *)
let check_global env global =
	if (string_of_vtype global.vtype) = "void" then raise (Failure("cannot use void as variable type"))
	(*  ret is new env *)
	else let ret = add_global global.vname global.vtype env in
	if StringMap.is_empty ret then raise (Failure ("global variable " ^ global.vname ^ " is already defined"))
	(* update the env with globals from ret *)
	else let env = {locals = env.locals; globals = ret; functions = env.functions } in
	convert_to_sast_type global env, env

let rec check_globals env globals = 
	match globals with
	  [] -> []
	| hd::tl -> let g, e = (check_global env hd) in (g, e)::(check_globals e tl)

let check_program (globals, funcs) = 
	(* create the default environment *)
 	let env = {	locals = StringMap.empty;
				globals = StringMap.empty;
				functions = StringMap.empty }
	in
	(* return the list of each global appended with its environments, the last global has the final env *)
	let g = check_globals env globals in
	(* make a list of globals *)
	let globals = List.map (fun global -> fst global) g in
	match g with
	(* no globals *)
	 [] -> (globals, (check_functions env (List.rev funcs)))
	(* get the envirnment from the last global *)
	| _ -> let e = snd (List.hd (List.rev g)) in (globals, (check_functions e (List.rev funcs)))



