open Parser
open Scanner
let parse lexbuf =
  Parser.main Scanner.token lexbuf

