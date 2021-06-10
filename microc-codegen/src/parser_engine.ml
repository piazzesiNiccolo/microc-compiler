open Util
open Lexing

module I = Parser.MenhirInterpreter

let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"
    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (Errors.message (I.number state)) with
        | Not_found -> "invalid syntax"

let rec parse_buf lexbuf (checkpoint : Ast.program I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Scanner.token lexbuf in
      let startp= lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse_buf lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse_buf lexbuf checkpoint
  | I.HandlingError _env ->
      let err = get_parse_error _env in
      Util.raise_syntax_error lexbuf err
  | I.Accepted v -> v
  | I.Rejected ->
       Util.raise_syntax_error lexbuf "Input rejected by the parser"

let parse lexbuf = parse_buf lexbuf (Parser.Incremental.program lexbuf.lex_curr_p) 
  
  