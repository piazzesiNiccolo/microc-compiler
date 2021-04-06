open Parser
open Scanner
let parse lexbuf =
  Parser.program Scanner.token lexbuf
