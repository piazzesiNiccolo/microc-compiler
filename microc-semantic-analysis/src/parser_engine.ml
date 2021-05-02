open Parser
open Scanner
let parse lexbuf =
try  
  Parser.program Scanner.token lexbuf
with Parser.Error ->
  Util.raise_syntax_error lexbuf @@ "Syntax error at lexeme  " ^Lexing.lexeme lexbuf 
