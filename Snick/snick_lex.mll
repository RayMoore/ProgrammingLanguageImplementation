(* ----------------------------------------------------- | 
 * Lexer for Snick language                              |
 * ----------------------------------------------------- |
 * Reads from OCaml input channel and returns tokens for |
 * processing by Snick parser                            |
 * ----------------------------------------------------- | *)

{
open Snick_parse

(* Define Lexing error messages *)
exception Lex_error of string

(* Gets the line number and column of current lexeme *)
let get_lex_pos lexbuf =
  let pos   = lexbuf.Lexing.lex_curr_p in
  let line  = pos.Lexing.pos_lnum  in
  let col   = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  (line, col)
let ln = ref 1 
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let digits = digit+
let ident = (alpha | '_') alnum*
let str = '"' [^ '\n' '\t' '"']* '"'
rule token = parse
  (* Skip blanks *)
  | [' ' '\t']    { token lexbuf }
  (* Newline *)
  | '\n'          { incr ln; Lexing.new_line lexbuf ; token lexbuf }
  (* Literals *)
  | '-'? ('0' | ['1'-'9']+['0'-'9']*) '.' digits as lxm { FLOAT_CONST lxm }
  | '-'? ('0' | ['1'-'9']+['0'-'9']*) as lxm { INT_CONST(int_of_string lxm) }
  (* Skip comments *)
  | '#'[^ '\n']* { token lexbuf } 
  | str as lxm { STR_CONST lxm }
  (* Keywords *)
  | "and" { AND !ln}
  | "do" { DO !ln}
  | "od" { OD !ln}
  | "if" { IF !ln}
  | "fi" { FI !ln}
  | "end" { END !ln}
  | "else" { ELSE !ln}
  | "not" {NOT !ln}
  | "or" { OR !ln}
  | "proc" { PROC !ln}
  | "ref" { REF !ln}
  | "then" { THEN !ln}
  | "val" { VAL !ln}
  | "while" { WHILE !ln}
  | "bool" { BOOL !ln}
  | "int" { INT !ln}
  | "float" { FLOAT !ln}
  | "true" { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "read" { READ !ln}
  | "write" { WRITE !ln}
  (* Symbols *)
  | ":=" { ASSIGN !ln}
  | '(' { LPAREN !ln}
  | ')' { RPAREN !ln}
  | '[' { LSQBRACKET !ln}
  | ']' { RSQBRACKET !ln}
  | ".." { DOUBLEDOT !ln}
  | ',' { COMMA !ln}
  (* Operators *)
  | "!=" { NOTEQ !ln}
  | '=' { EQ !ln}
  | "<=" { LTEQ !ln}
  | '<' { LT !ln}
  | ">=" { GTEQ !ln}
  | '>' { GT !ln}
  | '+' { PLUS !ln}
  | '-' { MINUS !ln}
  | '*' { MUL !ln}
  | '/' { DIV !ln}
  | ';' { SEMICOLON !ln}
  (* Ident is here so it won't interfere with matching keywords *)
  | ident as lxm { IDENT (!ln ,lxm) }
  | eof { EOF }
  | _ { raise (Lex_error
              ("Unknown symbol \"" ^ (Lexing.lexeme lexbuf) ^ "\"")) }
