(* ----------------------------------------------------- | 
 * Abstract Syntax Tree for Snick language               |
 * ----------------------------------------------------- |
 * Tree representation of Snick program in program       |
 * built by the Snick parser                             |
 * ----------------------------------------------------- | *)
(*int for line number*)
type ident = int * string

type snick_type = INT_TYPE|FLOAT_TYPE|BOOL_TYPE|NONE_TYPE
type arg_type = VAL_TYPE|REF_TYPE|LOCAL_TYPE

(* Keep aliases intact for pretty printing. *)
type snicktype =
  | Bool of int 
  | Int of int 
  | Float of int 
  | None

type arg_pass_type = 
  | Val of int 
  | Ref of int 
  | Local of int 

type binop =
  | Op_add of int
  | Op_sub of int
  | Op_mul of int
  | Op_div of int
  | Op_eq of int
  | Op_lt of int
  | Op_gt of int
  | Op_noteq of int
  | Op_gteq of int
  | Op_lteq of int
  | Op_and of int
  | Op_or of int

type unop =
  | Op_minus of int
  | Op_not of int

(* Mutually recursive types expr and lvalue *)
type expr =
  | Ebool of bool
  | Eint of int
  | Efloat of string
  | Estring of string
  | Elval of lvalue
  | Ebinop of binopExpr
  | Eunop of unopExpr
and lvalue =
  | LId of ident
  | LArray of (ident * expr list)
and binopExpr = (expr * binop * expr)
and unopExpr = (unop * expr)

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

(* First int is lower bound, Second int is upper bound *)
type interval = 
  | Interval of (int * int)

type decl = 
  | RegDecl of (ident * snicktype)
  | ArrayDecl of (ident * snicktype * interval list)

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | Ifthen of (expr * stmt list)
  | IfthenElse of (expr * stmt list * stmt list)
  | WhileDo of (expr * stmt list)
  | ProcCall of (ident * expr list)

type symbol_table = 
  | T_indicator of arg_pass_type
  | T_snicktype of snicktype
  | T_int of int
  | T_array_interval of interval list
  | T_array_value of expr list
  | St_proc of (string, symbol_table) Hashtbl.t (*key val pair for local variable or args*)


type arg = 
  |BasicSnickType of (arg_pass_type * snicktype * ident)
  |ArraySnickType of (arg_pass_type * snicktype * ident * expr list)

type proc_body = (decl list * stmt list)

type proc = (ident * arg list * proc_body)

type program = {
  procs : proc list
}
 
type t = program
