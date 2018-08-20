(* Team : zz *)
(* Authors : Molin Zhao --usrname: molinz --ID: 710830 *)
(*           Ruoqiao Zhang --usrname: ruoqiaoz --ID: 803950 *)
open Snick_ast
open Snick_symbol
open Snick_codegen

(* lookup proc name by proc id *)
let main_proc = "main"
let current_proc_table = ref (Hashtbl.create 1)
let current_expr_type = ref NONE_TYPE
let outer_expr_type = ref NONE_TYPE


let get_array_from_table symbol_table = match symbol_table with
	|T_array_interval(interval_list) -> interval_list

(*given a unknown snickType and print it out with a indicate message*)
let print_out_snick_type snick_type  = 
	match snick_type with
	|BOOL_TYPE-> "'Bool'"
	|INT_TYPE -> "'Int'"
	|FLOAT_TYPE -> "'Float'"
	|NONE_TYPE -> "'None'"


let get_value_from_expr expr = 
	match expr with
	|Eint(value) -> value
	|_ -> (-1)

let print_out_args_pass_type argpasstype indicate_msg = 
	match argpasstype with
	|Val(line) -> "'Val'"
	|Ref(line) -> "'Ref'"
	|Local(line)-> "'Local'"



(*table lookup function for finding the type of a given identity name, like x or y*)
let get_symbol_hash_table_primitive_type hash_table key_name = 
	if(Hashtbl.mem hash_table key_name) then (
		match (Hashtbl.find hash_table key_name) with
  		| T_snicktype(snicktype) -> snicktype
    	| _ -> None
	)else(
		None
	)

let check_array_expr_type ident expr= 
	let (line,id) = ident in
	match expr with
	|Eint(int) -> INT_TYPE
	|Efloat(string) -> FLOAT_TYPE
	|Ebool(bool) -> BOOL_TYPE
	|_ -> Printf.printf "array %s interval should be interger on line %d, but given expression\n" id line;failwith "16"

let check_array_bounds ident expr_list = 
	let pos = ref 0 in
	let (line,id) = ident in
	let array_interval_list = get_array_from_table (Hashtbl.find (get_table (Hashtbl.find !current_proc_table proc_local_array_table)) id) in(* return interval information *) 
	List.iter(fun expr->
		let expr_type = check_array_expr_type ident expr in
		if expr_type != INT_TYPE then (Printf.printf "array %s interval should be interger on line %d, but given %s\n" id line (print_out_snick_type expr_type);failwith "17")
		else (
			let expr_value = get_value_from_expr expr in
			try
				let interval = List.nth array_interval_list !pos in
				match interval with
				|Interval(left_indice,right_indice) -> 
				if expr_value < left_indice||expr_value >right_indice then (Printf.printf "manipulate array %s out of bounds on line %d\nwith indice of %d\n" id line expr_value;failwith "19") else (incr pos)
			with Failure e ->
				Printf.printf "manipulate array %s dimensions unmatch on line %d\n" id line;failwith "18"
		)
	) expr_list;
	true


let get_lvalue_line_number lvalue = 
	match lvalue with
	|LId(ident)->
		let (line,id) = ident in
		line
	|LArray(ident,expr_list)->
		(* should check bounds when get elements from array *)
		let (line,id) = ident in
		check_array_bounds ident expr_list;
		line

let get_line_num_by_snickType snickType  = 
	match snickType with
	|Bool(line) -> line
	|Int(line) -> line
	|Float(line) -> line
	|None -> Printf.printf "failed to get line number\n"; failwith "1"

(*given a unknown snick.ast token this method can match its identity name, then lookup its type in the symbol table*)
let rec get_lvalue_type hash_table lvalue = match lvalue with
    | LId(ident) -> 
    	let (line,id) = ident in
    	get_symbol_hash_table_primitive_type hash_table id
    | LArray(ident, expr_list) -> 
    	(* should check bounds first when get elements from array *)
    	let (line,id) = ident in
    	let temp_type = get_symbol_hash_table_primitive_type hash_table id in
    	if temp_type = None then None else (
    		(* if array exists, then check bounds *)
    		if (check_array_bounds ident expr_list) then temp_type else (Printf.printf "out of bounds on line %d\n" line; failwith "2"; None)
    	)



let get_lvalue_type_from_tables lvalue = 
	let line = get_lvalue_line_number lvalue in
	let lval_type = convert_snickType_to_basicType (get_lvalue_type (get_table (Hashtbl.find !current_proc_table proc_local_type_table)) lvalue) in 
	if lval_type != NONE_TYPE then (lval_type) else (
		let lval_type = convert_snickType_to_basicType (get_lvalue_type (get_table (Hashtbl.find !current_proc_table proc_param_type_table)) lvalue) in
		if lval_type != NONE_TYPE then lval_type else (Printf.printf "variable not found on line %d\n" line;failwith "3";NONE_TYPE)
	)

let get_binop_line_number binop =
	match binop with
	| Op_eq(line) -> line
	| Op_lt(line) -> line
	| Op_gt(line) -> line
	| Op_noteq(line) -> line
  	| Op_gteq(line) -> line
  	| Op_lteq(line) -> line
  	| Op_add(line) -> line
	| Op_sub(line) -> line
	| Op_mul(line) -> line
	| Op_div(line) -> line
	| Op_and(line) -> line
	| Op_or(line) -> line

let get_binop_type binop = 
	match binop with
	| Op_eq(line) -> "'='"
	| Op_lt(line) -> "'<'"
	| Op_gt(line) -> "'>'"
	| Op_noteq(line) -> "'!='"
  	| Op_gteq(line) -> "'>='"
  	| Op_lteq(line) -> "'<='"
  	| Op_add(line) -> "'+'"
	| Op_sub(line) -> "'-'"
	| Op_mul(line) -> "'*'"
	| Op_div(line) -> "'/'"
	| Op_and(line) -> "'and'"
	| Op_or(line) -> "'or'"

let get_type_from_snickType snicktype = 
	match snicktype with
  	|Bool(line) -> BOOL_TYPE
  	|Int(line) -> INT_TYPE
  	|Float(line) -> FLOAT_TYPE
  	|None -> NONE_TYPE
  	|_ -> Printf.printf "unknown type\n";failwith "4"


(*check if the binop is one of the flowing operations =, >, <, >=, <=, !=*)
let check_binop_is_cmp binop = match binop with 
	| Op_eq(line) -> true
	| Op_lt(line) -> true
	| Op_gt(line) -> true
	| Op_noteq(line) -> true
  	| Op_gteq(line) -> true
  	| Op_lteq(line) -> true
  	| _ -> false

(*check if the binop is +,-,*,/*)
let check_binop_is_arithmatic binop = match binop with
	| Op_add(line) -> true
	| Op_sub(line) -> true
	| Op_mul(line) -> true
	| Op_div(line) -> true
	| _ -> false	

(*check if the binop is 'and' or 'or'*)
let check_binop_is_and_or binop = match binop with
	| Op_and(line) -> true
	| Op_or(line) -> true
	| _ -> false


(*
 *check the type of a given expression
 *input: an expression
 *return: Int, Float, Bool
 *)
let rec check_expr_type expr = 
	match expr with
		| Eint(int) ->  INT_TYPE
		| Efloat(string) -> FLOAT_TYPE
		| Ebool(bool) -> BOOL_TYPE
		| Ebinop(expr1,binop,expr2) -> 
			(
				let line = get_binop_line_number binop in
				let binop_type = get_binop_type binop in
				let expr1_result_type = check_expr_type expr1 in let expr2_result_type = check_expr_type expr2 in
				(* if binop is 'and' or 'or' then the left expression and the right expression must be Bool*)
				if check_binop_is_and_or binop then (if expr1_result_type = BOOL_TYPE && expr2_result_type = BOOL_TYPE then BOOL_TYPE else (Printf.printf "type unmatch for %s on line %d\n" binop_type line;failwith "5";NONE_TYPE))
				(* if binop is arithmatic opertion or comparision then the left expr and right expr must be Int or Float *)
				else (if (expr1_result_type = BOOL_TYPE || expr2_result_type = BOOL_TYPE) then (Printf.printf "type unmatch for %s on line %d\n" binop_type line;failwith "6";NONE_TYPE)else
						(if check_binop_is_arithmatic binop then ( if expr1_result_type = INT_TYPE && expr2_result_type = INT_TYPE then INT_TYPE else FLOAT_TYPE )
						else BOOL_TYPE)
					)
			)
		| Eunop(unop,expr) -> 
			(
				let expr_result_type = check_expr_type expr in
				match unop with 
				| Op_minus(line) -> (* in this case expr can only be float or int *)
					if expr_result_type = FLOAT_TYPE || expr_result_type = INT_TYPE then expr_result_type else (Printf.printf "type unmatch for '-' on line %d\nexpected 'Int' or 'Float' but given %s\n" line (print_out_snick_type expr_result_type);failwith "7";NONE_TYPE)
  				| Op_not(line) -> (* in this case expr can only be true or false *)
  					if expr_result_type = BOOL_TYPE then expr_result_type else (Printf.printf "type unmatch for 'not' on line %d\nexpected 'Bool' but given %s\n" line (print_out_snick_type expr_result_type);failwith "8";NONE_TYPE)
  			)
		| Elval(lvalue) -> 
			get_lvalue_type_from_tables lvalue
		| _ -> NONE_TYPE

let check_array_interval interval_list line= 
	List.iter(fun interval ->
		match interval with
		|Interval(left_indice,right_indice)->
			if (left_indice > right_indice) then (Printf.printf "interval %d..%d declaration error on line %d\nfor interval m..n, m<=n" left_indice right_indice line;failwith "9")
	)interval_list

let translate_decls decl_list = List.iter (fun decl ->
	match decl with 
	| RegDecl(ident, snicktype) -> ()
	| ArrayDecl(ident,snicktype,interval_list) -> 
		(*array bounds checking, for float point[0..4,2..3,0..1] each interval, the left indice must be great than the right indice*)
		let (line, id) = ident in
		check_array_interval interval_list line
)decl_list

let get_rvalue rvalue = match rvalue with
	|Rexpr (expr) -> expr
	
(*outer_expr_type is being used in expr type checking method check_expr_type*)
let translate_stmts stmt_list = 
	List.iter 
	(
		fun stmt ->
		match stmt with 
		|Write (expr) ->
			(
				(* print_write_comment(); *)
				current_expr_type := NONE_TYPE; 
				outer_expr_type := NONE_TYPE;
				match expr with 
				| Estring(string) ->  ()
				| _ -> 
					let expr_result_type = check_expr_type expr in
					outer_expr_type := expr_result_type;
					(* Printf.printf "expression outer type: %s" (print_out_snick_type !outer_expr_type);
					Printf.printf "\n" *)
		)
	  	|Assign(lvalue, rvalue) -> 
	  		(
	  			let line = get_lvalue_line_number lvalue in
	  			let lval_type = get_lvalue_type_from_tables lvalue in 
	  			let rval_type = check_expr_type (get_rvalue rvalue) in
	  			(if lval_type = rval_type 
	  				then ((* do assign *)) 
	  			else (
	  				if lval_type = FLOAT_TYPE && rval_type = INT_TYPE then ((* do assign *)) 
	  				else(
	  					Printf.printf "cannot assign with unmatched types, %s ':=' %s on line %d\n" (print_out_snick_type lval_type) (print_out_snick_type rval_type) line;failwith "10")
	  				)
	  			)
	  		)
	  	|Read(lvalue) -> 
	  		(
	  			let lval_type = get_lvalue_type_from_tables lvalue in()
	  		)
	  	|Ifthen(expr,stmt_list) ->
	  		(
	  			let expr_result_type = check_expr_type expr in
	  			if expr_result_type = BOOL_TYPE then (* expr is a Bool type but need to lookup if it is true or false*) ()
	  			else (Printf.printf "if statements expect a expression of Bool, but given a %s\n" (print_out_snick_type expr_result_type);failwith "11")
	  		)
	  	|IfthenElse(expr,stmt_list_1,stmt_list_2) ->
	  		(
	  			let expr_result_type = check_expr_type expr in
	  			if expr_result_type = BOOL_TYPE then (* expr is a Bool type but need to lookup if it is true or false*) ()
	  			else (Printf.printf "if statements expect a expression of Bool, but given a %s\n" (print_out_snick_type expr_result_type);failwith "12")
	  		)
	  	|WhileDo(expr,stmt_list) -> 
	  		(
	  			let expr_result_type = check_expr_type expr in
	  			if expr_result_type = BOOL_TYPE then (* expr is a Bool type but need to lookup if it is true or false*) ()
	  			else (Printf.printf "while statements expect a expression of Bool, but given a %s\n" (print_out_snick_type expr_result_type);failwith "13")
	  		)
	  	|ProcCall(id,expr_list) -> 
	  		(
	  			(*proc function call, check the ident and declares parameters
				 *if all matched, then translate this table
				 *else raise exception for proc not found or parameters unmatched
	  			 *)
	  			 let (line, ident) = id in
	  			 let this_proc_table = get_table(Hashtbl.find all_proc_table ident) in
	  			 let this_proc_param_indicator_table = get_table(Hashtbl.find this_proc_table proc_param_indicator_table) in
	  			 let this_proc_param_type_table = get_table(Hashtbl.find this_proc_table proc_param_type_table) in
	  			 let this_proc_param_slot_table = get_table(Hashtbl.find this_proc_table proc_param_slot_table) in
	  			 let this_convert_proc_param_slot_table = Hashtbl.create (Hashtbl.length this_proc_param_slot_table) in
	  			 Hashtbl.iter(fun key value -> 
	  			 	Hashtbl.add this_convert_proc_param_slot_table value key;
	  			 ) this_proc_param_slot_table;
	  			 let slot_count = ref 0 in
	  			 List.iter(fun e -> 
					let e_type = check_expr_type e in
					let param_ident = Hashtbl.find this_convert_proc_param_slot_table (T_int(!slot_count)) in
					let corresponding_param_type = get_type_from_snickType (get_snick_type (Hashtbl.find this_proc_param_type_table param_ident)) in 
					if(e_type = corresponding_param_type) then () else (Printf.printf "%s() parameters type unmatch with the %dth on line %d\n" ident (!slot_count+1) line;failwith "14");
					incr slot_count;
	  			 )expr_list;
	  			 let this_pro_param_count = Hashtbl.length this_proc_param_slot_table in
	  			 if !slot_count != this_pro_param_count then (Printf.printf "%s() has %d parameters but given %d on line %d\n" ident this_pro_param_count !slot_count line;failwith "15")
	  		)
	) stmt_list
	  			

let translate_body proc_body = match proc_body with 
	|(decl_list, stmt_list) -> (translate_decls decl_list; translate_stmts stmt_list)

let translate_proc proc = match proc with 
	|(ident, arg_list,proc_body) -> 
		(
			let (line, id) = ident in
			translate_body proc_body;
		)


(* called by snick.ml for compling *)
let begin_prog prog = 
	(* Create symbol table for all procs *)
	all_proc_table_insert prog.procs;
	current_proc_table := get_table(Hashtbl.find all_proc_table main_proc);
	(* Check symbol table, exit if error *)

	(* Translate program starting by main proc *)
	translate_proc (find_main (prog.procs));
	(* Translate all other procs *)
	List.iter(fun proc -> 
		match proc with
			| (ident,_,(_,_)) -> 
				let (line, id) = ident in
	  			if not (id = main_proc) 
	  				then ( 
	  					current_proc_table := get_table(Hashtbl.find all_proc_table id);
	  					translate_proc proc
	  				)
	) (prog.procs);

	codegen_program prog

