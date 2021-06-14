exception Syntax_error of string

exception Lexing_error of string

exception Semantic_error of string

exception Codegen_error of string
open Ast

let rt_support =
  [
    ( "print",
      ( dummy_pos,
        {
          typ = TypV;
          fname = "print";
          formals = [ (TypI, "a") ];
          body = { loc = dummy_pos; node = Block []; id = 0 };
        } ) );
    ( "getint",
      ( dummy_pos,
        {
          typ = TypI;
          fname = "getint";
          formals = [];
          body = { loc = dummy_pos; node = Block []; id = 0 };
        } ) );
     ( "getfloat",
      ( dummy_pos,
        {
          typ = TypF;
          fname = "getfloat";
          formals = [];
          body = { loc = dummy_pos; node = Block []; id = 0 };
        } ) );
     ( "getcharacter",
      ( dummy_pos,
        {
          typ = TypC;
          fname = "getcharacter";
          formals = [];
          body = { loc = dummy_pos; node = Block []; id = 0 };
        } ) );
    ( "printchar",
      ( dummy_pos,
        {
          typ = TypV;
          fname = "printchar";
          formals = [ (TypC, "c") ];
          body = { loc = dummy_pos; node = Block []; id = 0 };
        } ) );
    ( "printfloat",
      ( dummy_pos,
        {
          typ = TypF;
          fname = "printfloat";
          formals = [ (TypF, "f") ];
          body = { loc = dummy_pos; node = Block []; id = 0 };
        } ) );
  ]

let rec string_of_type  = function
  | TypI -> "int" 
  | TypC -> "char"
  | TypF -> "float"
  | TypB -> "bool"
  | TypV -> "void"
  | TypS(s) -> "struct "^s
  | TypP t1 -> "*"^string_of_type t1
  | TypNull -> "null"
  | TypA(t, v) -> string_of_type t ^ "["^( Option.fold ~none:"" ~some:string_of_int v) ^ "]"

let string_of_uop = function 
  
| Neg -> "-"
| Not -> "!"
| PreInc | PostInc -> "++"
| PreDec | PostDec -> "--"

let string_of_binop = function 
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Comma -> ","

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

let raise_syntax_error lexbuf msg =
  let pos = get_lexing_position lexbuf in
  let m = Printf.sprintf "%d:%d: %s" (fst pos) (snd pos) msg in
  raise (Syntax_error m)

let raise_lexer_error lexbuf msg =
  let pos = get_lexing_position lexbuf in
  let m = Printf.sprintf "%d:%d: %s" (fst pos) (snd pos) msg in
  raise (Lexing_error m)

let raise_semantic_error (startp, endp) msg =
  let start_line_number = startp.Lexing.pos_lnum in
  let start_column_number =
    startp.Lexing.pos_cnum - startp.Lexing.pos_bol + 1
  in
  let end_line_number = endp.Lexing.pos_lnum in
  let end_column_number = endp.Lexing.pos_cnum - endp.Lexing.pos_bol + 1 in
  let line =
    if start_line_number = end_line_number then string_of_int start_line_number
    else Printf.sprintf "%d-%d" start_line_number end_line_number
  in
  let column =
    if start_column_number = end_column_number then
      string_of_int start_column_number
    else Printf.sprintf "%d-%d" start_column_number end_column_number
  in
  let log = Printf.sprintf "%s:%s: %s" line column msg in
  raise (Semantic_error log)

let raise_codegen_error msg = raise (Codegen_error msg)
