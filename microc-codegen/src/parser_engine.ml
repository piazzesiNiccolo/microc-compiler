open Parser
Open Scanner
let parse lexbuf =
  Parser.main Scanner.next_token lexbuf

