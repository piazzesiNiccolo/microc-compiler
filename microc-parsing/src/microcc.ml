
let () =
  let ci = open_in filename in
  let lb = Lexing.from_channel ci in
    try
      let p =Scanner.token lb in
      Printf.printf "OK\n"

    with
    | Util.Lexing_error(m) ->
      Printf.fprintf stderr "Lexing error:\n%s:%s\n" !filename m; exit 1
    | Util.Syntax_error(m) ->
      Printf.fprintf stderr "Syntax error:\n%s:%s\n" !filename m; exit 2
    | Util.Semantic_error(m) ->
      Printf.fprintf stderr "Error:\n%s:%s\n" !filename m; exit 3
