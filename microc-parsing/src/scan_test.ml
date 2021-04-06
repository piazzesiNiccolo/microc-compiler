open Scanner
let  rec scan lexbuf = match Scanner.token lexbuf with
| EOF -> Printf.printf "ciao"
| _ -> scan lexbuf

let () = 
  let filename = Sys.argv.(1) in
  let in_channel = open_in filename in 
  let lexbuf = Lexing.from_channel in_channel ~with_positions:true in
  scan lexbuf;
  
  


