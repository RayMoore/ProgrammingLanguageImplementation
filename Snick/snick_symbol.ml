(* Team : zz *)
(* Authors : Molin Zhao --usrname: molinz --ID: 710830 *)
(*           Ruoqiao Zhang --usrname: ruoqiaoz --ID: 803950 *)
open Snick_ast

let param_size = ref (-1)
let local_size = ref (-1)
let table_size = 255
let all_proc_table = Hashtbl.create table_size
let proc_stack_size_table = Hashtbl.create table_size

let proc_param_indicator_table = "proc_param_indicator_table"
let proc_param_type_table = "proc_param_type_table"
let proc_param_slot_table = "proc_param_slot_table"
let proc_param_array_table = "proc_param_array_table"
let proc_local_indicator_table = "proc_local_indicator_table"
let proc_local_type_table = "proc_local_type_table"
let proc_local_slot_table = "proc_local_slot_table"
let proc_local_array_table = "proc_local_array_table"

type can_compile = TRUE|FALSE
let compile = ref TRUE

(*for printing out error message then exceptions occur*)
let failwith msg = 
	compile := FALSE;raise (Failure msg);exit 0

let get_snick_type symbol_hash_table = match symbol_hash_table with
	|T_snicktype(snicktype) -> snicktype

let get_slot_num symbol_hash_table = match symbol_hash_table with
	|T_int(int) -> int
	
let get_table symbol_hash_table = match symbol_hash_table with 
	| St_proc(tbl) -> tbl 
	| _ ->  failwith "get symbol hash failed\n"

(*insert local variables*)
let proc_decl_table_insert proc_table decl_list = List.iter(fun decl ->(
	incr local_size;
	match decl with 
	|ArrayDecl (ident,snicktype,interval_list)-> 
		let (line, id) = ident in
		(*check ident in param table, make sure that this ident is available locally*)
		if((Hashtbl.mem (get_table (Hashtbl.find proc_table proc_param_indicator_table)) id) = false) then(
			Hashtbl.add (get_table (Hashtbl.find proc_table proc_local_type_table)) id (T_snicktype(snicktype));
			Hashtbl.add (get_table (Hashtbl.find proc_table proc_local_slot_table)) id (T_int(!local_size));
			Hashtbl.add (get_table (Hashtbl.find proc_table proc_local_indicator_table)) id (T_indicator(Local(line)));
			Hashtbl.add (get_table (Hashtbl.find proc_table proc_local_array_table)) id (T_array_interval(interval_list))
		)else(
			Printf.printf "%s redifined on line %d\n" id line;
			failwith ""
		)
	|RegDecl (ident,snicktype) -> 
		let (line, id) = ident in
		(*check ident in param table, make sure that this ident is available locally*)
		if((Hashtbl.mem (get_table (Hashtbl.find proc_table proc_param_indicator_table)) id) = false) then(
			Hashtbl.add (get_table (Hashtbl.find proc_table proc_local_type_table)) id (T_snicktype(snicktype));
			Hashtbl.add (get_table (Hashtbl.find proc_table proc_local_slot_table)) id (T_int(!local_size));
			Hashtbl.add (get_table (Hashtbl.find proc_table proc_local_indicator_table)) id (T_indicator(Local(line)))
		)else(
			Printf.printf "%s redifined on line %d\n" id line;
			failwith ""
		)
)) decl_list

(*insert params*)
let proc_arg_table_insert proc_table arg_list = List.iter(fun arg ->(
	incr param_size;
	match arg with 
	|BasicSnickType(arg_pass_type, snicktype, id) -> 
		let (line, ident) = id in
		Hashtbl.add (get_table (Hashtbl.find proc_table proc_param_indicator_table)) ident (T_indicator(arg_pass_type));
		Hashtbl.add (get_table (Hashtbl.find proc_table proc_param_type_table)) ident (T_snicktype(snicktype));
		Hashtbl.add (get_table (Hashtbl.find proc_table proc_param_slot_table)) ident (T_int(!param_size));
	|ArraySnickType(arg_pass_type,snicktype,id,expr_list) ->
		let (line, ident) = id in
		Hashtbl.add (get_table (Hashtbl.find proc_table proc_param_indicator_table)) ident (T_indicator(arg_pass_type));
		Hashtbl.add (get_table (Hashtbl.find proc_table proc_param_type_table)) ident (T_snicktype(snicktype));
		Hashtbl.add (get_table (Hashtbl.find proc_table proc_param_slot_table)) ident (T_int(!param_size));
		Hashtbl.add (get_table (Hashtbl.find proc_table proc_param_array_table)) ident (T_array_value(expr_list));
)) arg_list


let all_proc_table_insert procs = List.iter (fun proc -> (
	local_size:=-1;
	param_size:=-1;
	match proc with 
		(*add proc to all_proc_table
		 *key: identity
		 *value: St_proc tbl
		 *)
		|(ident, arg_list, (decl_list,stmt_list)) -> 
		(* for array, only one element of an array can be a passed in a proc *)
		let (line, id) = ident in
		(Hashtbl.add all_proc_table id (St_proc(Hashtbl.create table_size));
		(*init proc param table*)
		let this_proc_table = get_table(Hashtbl.find all_proc_table id) in (*ident,symbol_table*)
		Hashtbl.add this_proc_table proc_param_indicator_table (St_proc(Hashtbl.create table_size));
		Hashtbl.add this_proc_table proc_param_slot_table (St_proc(Hashtbl.create table_size));
		Hashtbl.add this_proc_table proc_param_type_table (St_proc(Hashtbl.create table_size));
		Hashtbl.add this_proc_table proc_param_array_table (St_proc(Hashtbl.create table_size));
		(*init proc local table*)
		Hashtbl.add this_proc_table proc_local_indicator_table (St_proc(Hashtbl.create table_size));
		Hashtbl.add this_proc_table proc_local_slot_table (St_proc(Hashtbl.create table_size));
		Hashtbl.add this_proc_table proc_local_type_table (St_proc(Hashtbl.create table_size));
		Hashtbl.add this_proc_table proc_local_array_table (St_proc(Hashtbl.create table_size));
		
		(* Create tables to store attributes for all parameters *)
		proc_decl_table_insert this_proc_table decl_list;
		proc_arg_table_insert this_proc_table arg_list;
		Hashtbl.add proc_stack_size_table id ((!local_size+1)+(!param_size+1))
	))) procs
