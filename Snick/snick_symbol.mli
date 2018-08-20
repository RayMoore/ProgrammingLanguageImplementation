val local_size : int ref
val param_size : int ref
val table_size : int
val all_proc_table : (string, Snick_ast.symbol_table) Hashtbl.t
val proc_stack_size_table : (string,int) Hashtbl.t
val proc_param_indicator_table : string
val proc_param_type_table : string
val proc_param_slot_table : string
val proc_param_array_table: string
val proc_local_indicator_table : string
val proc_local_type_table : string
val proc_local_slot_table : string
val proc_local_array_table: string

val get_slot_num: Snick_ast.symbol_table -> int
val get_snick_type: Snick_ast.symbol_table -> Snick_ast.snicktype
val get_table : Snick_ast.symbol_table -> (string, Snick_ast.symbol_table) Hashtbl.t
val proc_decl_table_insert : (string, Snick_ast.symbol_table) Hashtbl.t -> Snick_ast.decl list -> unit
val proc_arg_table_insert: (string, Snick_ast.symbol_table) Hashtbl.t -> Snick_ast.arg list -> unit
val all_proc_table_insert : (('a * string) * Snick_ast.arg list * (Snick_ast.decl list * 'b))
           list -> unit
(* val all_proc_table_insert : (Snick_ast.ident *
            (Snick_ast.arg_pass_type * Snick_ast.snicktype * Snick_ast.ident)
            list * (Snick_ast.decl list * 'a))
           list -> unit *)


