(* Team : zz *)
(* Authors : Molin Zhao --usrname: molinz --ID: 710830 *)
(*           Ruoqiao Zhang --usrname: ruoqiaoz --ID: 803950 *)
open Snick_ast
open Snick_symbol

let print_halt () = Printf.printf "halt\n"
let print_return () = Printf.printf "return\n"

let to_read_int () = Printf.printf "call_builtin read_int\n"
let to_read_real () = Printf.printf "call_builtin read_real\n"
let to_read_bool () = Printf.printf "call_builtin read_bool\n"
let to_print_int () = Printf.printf "call_builtin print_int\n"
let to_print_bool () = Printf.printf "call_builtin print_bool\n"
let to_print_real () = Printf.printf "call_builtin print_real\n"
let to_print_string () = Printf.printf "call_builtin print_string\n"
let to_print_expr expr = Printf.printf "call_builtin print_expr %s\n" expr

let print_debug_reg register = Printf.printf "debug_reg %s\n" register
let print_debug_slot slot_num = Printf.printf "debug_slot %d\n" slot_num
let print_debug_stack ()  = Printf.printf "debug_stack\n"

let print_proc_call proc_ident = Printf.printf "call %s\n" proc_ident

let print_bt_func_call func_name = Printf.printf "call_builtin %s\n" func_name

let to_branch_on_true register proc_ident = Printf.printf "branch_on_true %s, %s\n" register proc_ident
let to_branch_on_false register proc_ident = Printf.printf "branch_on_false %s, %s\n" register proc_ident
let to_branch_oncond proc_ident = Printf.printf "branch_oncond %s\n" proc_ident

let print_push_stack_frame stack_size = Printf.printf "push_stack_frame %d\n" stack_size
let print_pop_stack_frame stack_size = Printf.printf "pop_stack_frame %d\n" stack_size

let to_register register_num = "r"^(string_of_int register_num)
let to_load register slot_num = Printf.printf "load %s, %d\n" register slot_num
let to_store slot_num register = Printf.printf "store %d, %s\n" slot_num register
let to_load_address register slot_num = Printf.printf "load_address %s, %d\n" register slot_num
let to_load_indirect reg_1 reg_2 =Printf.printf "load_indirect %s, %s\n" reg_1 reg_2
let to_store_indirect reg_1 reg_2 =Printf.printf "store_indirect %s, %s\n" reg_1 reg_2

let to_int_const register int_num = Printf.printf "int_const %s, %d\n" register int_num
let to_real_const register real_string = Printf.printf "real_const %s, %s\n" register real_string
let to_string_const register string_value = Printf.printf "string_const %s, %s\n" register string_value

let to_add_int reg_1 reg_2 reg_3 = Printf.printf "add_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_add_real reg_1 reg_2 reg_3 = Printf.printf "add_real %s, %s, %s\n" reg_1 reg_2 reg_3
let to_add_offset reg_1 reg_2 reg_3 = Printf.printf "add_offset %s, %s, %s\n" reg_1 reg_2 reg_3
let to_sub_int reg_1 reg_2 reg_3 = Printf.printf "sub_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_sub_real reg_1 reg_2 reg_3 = Printf.printf "sub_real %s, %s, %s\n" reg_1 reg_2 reg_3
let to_sub_offset reg_1 reg_2 reg_3 = Printf.printf "sub_offset %s, %s, %s\n" reg_1 reg_2 reg_3
let to_mul_int reg_1 reg_2 reg_3 = Printf.printf "mul_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_mul_real reg_1 reg_2 reg_3 = Printf.printf "mul_real %s, %s, %s\n" reg_1 reg_2 reg_3
let to_div_int reg_1 reg_2 reg_3 = Printf.printf "div_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_div_real reg_1 reg_2 reg_3 = Printf.printf "div_real %s, %s, %s\n" reg_1 reg_2 reg_3

let to_cmp_eq_int reg_1 reg_2 reg_3 = Printf.printf "cmp_eq_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_ne_int reg_1 reg_2 reg_3 = Printf.printf "cmp_ne_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_gt_int reg_1 reg_2 reg_3 = Printf.printf "cmp_gt_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_ge_int reg_1 reg_2 reg_3 = Printf.printf "cmp_ge_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_lt_int reg_1 reg_2 reg_3 = Printf.printf "cmp_lt_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_le_int reg_1 reg_2 reg_3 = Printf.printf "cmp_le_int %s, %s, %s\n" reg_1 reg_2 reg_3

let to_cmp_eq_real reg_1 reg_2 reg_3 = Printf.printf "cmp_eq_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_ne_real reg_1 reg_2 reg_3 = Printf.printf "cmp_ne_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_gt_real reg_1 reg_2 reg_3 = Printf.printf "cmp_gt_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_ge_real reg_1 reg_2 reg_3 = Printf.printf "cmp_ge_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_lt_real reg_1 reg_2 reg_3 = Printf.printf "cmp_lt_int %s, %s, %s\n" reg_1 reg_2 reg_3
let to_cmp_le_real reg_1 reg_2 reg_3 = Printf.printf "cmp_le_int %s, %s, %s\n" reg_1 reg_2 reg_3

let to_and reg_1 reg_2 reg_3 = Printf.printf "and %s, %s, %s\n" reg_1 reg_2 reg_3
let to_or reg_1 reg_2 reg_3 = Printf.printf "or %s, %s, %s\n" reg_1 reg_2 reg_3
let to_not reg_1 reg_2 = Printf.printf "not %s, %s\n" reg_1 reg_2

let to_int_to_real reg_1 reg_2 = Printf.printf "int_to_real %s, %s\n" reg_1 reg_2
let to_move reg_1 reg_2 = Printf.printf "move %s, %s\n" reg_1 reg_2

let print_write_comment () = Printf.printf "\n# write\n"
let print_proc_name proc_name = Printf.printf "%s:\n" proc_name
let register_count = ref 0
let codegen_this_proc_table = ref (Hashtbl.create 10)
let codegen_current_expr_type = ref None

let codegen_get_binop_type binop = 
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

let convert_snickType_to_basicType snicktype = 
	match snicktype with
	|Int(line) -> INT_TYPE
	|Float(line) -> FLOAT_TYPE
	|Bool(line) -> BOOL_TYPE
	|None -> NONE_TYPE

let codegen_get_symbol_hash_table_primitive_type hash_table key_name = 
	match (Hashtbl.find hash_table key_name) with
  	| T_snicktype(snicktype) -> snicktype
    | _ -> failwith "cannot get the type of "^key_name;None

(*given a unknown snick.ast token this method can match its identity name, then lookup its type in the symbol table*)
let rec codegen_get_lvalue_type hash_table lvalue = match lvalue with
    | LId(ident) -> 
    	let (line, id) = ident in
    	codegen_get_symbol_hash_table_primitive_type hash_table id
	| LArray(ident, expr_list) -> failwith "array type"
    | _ -> failwith "unknown type"


(*check if the binop is +,-,*,/*)
let codegen_check_binop_is_arithmatic binop = match binop with
	| Op_add(line) -> true
	| Op_sub(line) -> true
	| Op_mul(line) -> true
	| Op_div(line) -> true
	| _ -> false	

(*check if the binop is 'and' or 'or'*)
let codegen_check_binop_is_and_or binop = match binop with
	| Op_and(line) -> true
	| Op_or(line) -> true
	| _ -> false


let rec codegen_check_expr_type expr = 
	match expr with
		| Eint(int) ->  INT_TYPE
		| Efloat(string) -> FLOAT_TYPE
		| Ebool(bool) -> BOOL_TYPE
		| Ebinop(expr1,binop,expr2) -> 
				let binop_type = codegen_get_binop_type binop in
				let expr1_result_type = codegen_check_expr_type expr1 in let expr2_result_type = codegen_check_expr_type expr2 in
				(* if binop is 'and' or 'or' then the left expression and the right expression must be Bool*)
				if codegen_check_binop_is_and_or binop then (if expr1_result_type = BOOL_TYPE && expr2_result_type = BOOL_TYPE then BOOL_TYPE else (Printf.printf "type unmatch for %s on line 222\n" binop_type;failwith "";NONE_TYPE))
				(* if binop is arithmatic opertion or comparision then the left expr and right expr must be Int or Float *)
				else (if (expr1_result_type = BOOL_TYPE || expr2_result_type = BOOL_TYPE) then (Printf.printf "type unmatch for %s on line \n" binop_type;failwith "";NONE_TYPE)else
						(if codegen_check_binop_is_arithmatic binop then ( if expr1_result_type = INT_TYPE && expr2_result_type = INT_TYPE then INT_TYPE else FLOAT_TYPE )
						else BOOL_TYPE)
			)
		| Eunop(unop,expr) -> 
			(
				let expr_result_type = codegen_check_expr_type expr in
				match unop with 
				| Op_minus(line) -> (* in this case expr can only be float or int *)
					if expr_result_type = FLOAT_TYPE || expr_result_type = INT_TYPE then expr_result_type else (Printf.printf "type unmatch for '-' with "; NONE_TYPE)
  				| Op_not(line) -> (* in this case expr can only be true or false *)
  					if expr_result_type = BOOL_TYPE then expr_result_type else (Printf.printf "type unmatch for 'not' with "; NONE_TYPE)
  			)
		| Elval(ident) -> convert_snickType_to_basicType (codegen_get_lvalue_type (get_table (Hashtbl.find !codegen_this_proc_table proc_param_type_table)) ident)
		| _ -> NONE_TYPE

let codegen_print_binop binop outer_type = 
	let reg_count_1 = !register_count in 
	let reg_count_2 = !register_count+1 in
	match binop with
	| Op_add(line) -> if(outer_type=FLOAT_TYPE) then to_add_real 
	(to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2)) else to_add_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
	| Op_sub(line) -> if(outer_type=FLOAT_TYPE) then to_sub_real (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2)) else to_sub_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
	| Op_mul(line) -> if(outer_type=FLOAT_TYPE) then to_mul_real (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))else to_mul_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
	| Op_div(line) -> if(outer_type=FLOAT_TYPE) then to_div_real (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2)) else to_div_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
	| Op_eq(line)  -> if(outer_type=FLOAT_TYPE) then to_cmp_eq_real (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2)) else to_cmp_eq_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
	| Op_lt(line) -> if(outer_type=FLOAT_TYPE) then to_cmp_lt_real (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2)) else to_cmp_lt_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
	| Op_gt(line) -> if(outer_type=FLOAT_TYPE) then to_cmp_gt_real (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2)) else to_cmp_gt_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
	| Op_noteq(line) -> if(outer_type=FLOAT_TYPE) then to_cmp_ne_real (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2)) else to_cmp_ne_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
  	| Op_gteq(line) -> if(outer_type=FLOAT_TYPE) then to_cmp_ge_real (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2)) else to_cmp_ge_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
  	| Op_lteq(line) -> if(outer_type=FLOAT_TYPE) then to_cmp_le_real (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2)) else to_cmp_le_int (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
  	| Op_and(line) -> to_and (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))
  	| Op_or(line) -> to_or (to_register (reg_count_1)) (to_register (reg_count_1)) (to_register (reg_count_2))

let rec codegen_print_expr expr outer_type = 
	match expr with
	| Eint(int) -> 	incr register_count;
					if outer_type = FLOAT_TYPE then (to_int_const (to_register (!register_count)) int; to_int_to_real (to_register (!register_count)) (to_register (!register_count)); to_real_const (to_register (!register_count)) ((string_of_float (float_of_int int))^"0"))
					else to_int_const (to_register (!register_count)) int 
	| Ebool(false) -> incr register_count; to_int_const (to_register (!register_count)) 0
	| Ebool(true) -> incr register_count; to_int_const (to_register (!register_count)) 1
	| Efloat(string)-> to_real_const (to_register (!register_count)) string
	| Ebinop (expr1, binop, expr2)->  codegen_print_expr expr1 outer_type; 
									  codegen_print_expr expr2 outer_type; 
									  codegen_print_binop binop outer_type
	| Elval(lvalue) -> ()
let codegen_get_rvalue rvalue = match rvalue with
	|Rexpr (expr) -> expr

let codegen_stmts stmt_list = List.iter(fun stmt ->
	match stmt with
	| Assign (lvalue,rvalue) -> ()
  	| Read (lvalue) -> (match lvalue with
  		|LId(id) ->
  			let (line, ident) = id in 
  			let type_table = get_table (Hashtbl.find !codegen_this_proc_table proc_param_type_table) in
  			let lvalue_type = get_snick_type (Hashtbl.find type_table ident) in
  			(if(lvalue_type = Int(line)) then to_read_int () else (
  				if(lvalue_type = Float(line)) then to_read_real () else (
  					if(lvalue_type = Bool(line)) then to_read_bool()
  				)
  			);
  			let slot_table = get_table(Hashtbl.find !codegen_this_proc_table proc_param_slot_table) in 
  			let lvalue_slot = get_slot_num(Hashtbl.find slot_table ident) in 
  			to_store lvalue_slot (to_register(!register_count))
  			)
  		)
  	| Write (expr) -> 
  		(match expr with
  		|Estring (string) -> to_string_const (to_register (!register_count)) string; to_print_string()
  		|_ -> let outer_expr_type = codegen_check_expr_type expr in 
  				(codegen_print_expr expr outer_expr_type; 
  					if(outer_expr_type = INT_TYPE )then to_print_int() else 
					(if outer_expr_type = FLOAT_TYPE then to_print_real() else (if outer_expr_type = BOOL_TYPE then to_print_bool()))
  				)
  					
  				)
  	| Ifthen (expr,stmt_list) ->()
  	| IfthenElse (expr,stmt_list_1,stmt_list_2)->()
  	| WhileDo (expr,stmt_list) ->()
  	| ProcCall (ident,expr_list) ->()

)stmt_list

let codegen_param ident snicktype = 
	let decl_slot_table = get_table(Hashtbl.find !codegen_this_proc_table proc_param_slot_table ) in
	let decl_slot_num = get_slot_num(Hashtbl.find decl_slot_table ident) in 
	(
	match snicktype with
	|Int(line) -> to_int_const (to_register (!register_count)) 0; to_store decl_slot_num (to_register (!register_count))
	|Bool(line) -> to_int_const (to_register (!register_count)) 0; to_store decl_slot_num (to_register (!register_count))
	|Float(line) -> to_real_const (to_register (!register_count)) "0.0000"; to_store decl_slot_num (to_register (!register_count))
	)

let codegen_decls decls = List.iter ( fun decl ->
	match decl with
	| RegDecl (ident,snicktype) -> 
		let (line,id) = ident in
		codegen_param id snicktype
  	| ArrayDecl (ident,snicktype,interval_list) -> Printf.printf "An array here \n"
) decls

let codegen_proc_body proc_body = match proc_body with
	|(decl_list, stmt_list) -> codegen_decls decl_list; codegen_stmts stmt_list

let codegen_args arg_list = List.iter (fun arg ->
	match arg with
	|BasicSnickType(arg_pass_type,snicktype,ident) -> 
		let (line,id)  = ident in
		codegen_param id snicktype
	|ArraySnickType(arg_pass_type,snicktype,ident,expr_list) ->()

) arg_list

let codegen_proc proc = match proc with 
	|(ident, arg_list, proc_body) ->
		let (line, id)  = ident in 
		print_proc_name id; 
		print_push_stack_frame (Hashtbl.find proc_stack_size_table id);
		codegen_args arg_list; codegen_proc_body proc_body; 
		print_pop_stack_frame (Hashtbl.find proc_stack_size_table id);
		print_return()

let rec find_main procs = try match (List.hd procs) with
        | (ident,_,(_,_)) -> 
        	let (line, id) = ident in
        	if id = "main" then List.hd procs  
    		else find_main (List.tl procs)
        with Failure e-> (raise (Failure "no main function\n")) 

let codegen_program program = 
		codegen_this_proc_table := get_table(Hashtbl.find all_proc_table "main");
  		print_proc_call "main";
  		print_halt();
  		codegen_proc ((find_main program.procs));
  		List.iter(fun proc -> match proc with
  		| (id,_,(_,_)) -> 
  		let (line,ident) = id in 
  		if not (ident = "main") 
  		then ( 
  		codegen_this_proc_table := get_table(Hashtbl.find all_proc_table ident);
  		print_proc_name ident;
  		codegen_proc proc
  		)) (program.procs)

