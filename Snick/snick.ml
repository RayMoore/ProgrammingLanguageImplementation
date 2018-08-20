(* ----------------------------------------------------- | 
 * Main Module for Snick language Compiler               |
 * ----------------------------------------------------- |
 * This module takes a program file or stdin             |
 * and parses flags to decide whether to pretty print    |
 * or compile the provided Snick program                 |
 * ----------------------------------------------------- | *)

 (* Team : zz *)
 (* Authors : Molin Zhao --usrname: molinz --ID: 710830 *)
 (*           Ruoqiao Zhang --usrname: ruoqiaoz --ID: 803950 *)

module P = Snick_parse

(* Argument parsing code *)
let infile_name = ref None

type compiler_mode = Compile|PrettyPrint
let mode = ref Compile

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  ["-p",
     Arg.Unit(fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode"
  ]

let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "snick [-p] [snick source]" ;
  (* Open the input file *)
  let infile = 
    match !infile_name with
    | None -> stdin
    | Some fname -> open_in fname in
  (* Initialize lexing buffer *)
  let lexbuf = Lexing.from_channel infile in
  (* Call the parser *)
try
  let prog = Snick_parse.program Snick_lex.token lexbuf in
  match !mode with
  | Compile -> Snick_analyze.begin_prog prog
with
  (* Error reporting with line numbers and columns *)
  | Snick_lex.Lex_error msg -> (* Lexing error *)
      let (ln, col) = Snick_lex.get_lex_pos lexbuf in
      Printf.fprintf stderr
        "Lexer error: %s at line %i, column %i\n" msg ln col;
      exit 1
  | Parsing.Parse_error -> (* Parsing error *)
      let (ln, col) = Snick_lex.get_lex_pos lexbuf in
      Printf.fprintf stderr
        "Parse error at line %i, column %i\n" ln col;
      exit 1

let _ = main ()
