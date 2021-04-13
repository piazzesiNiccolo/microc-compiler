{
    open Parser
    open Lexing

    let create_hashtable size init =
        let table = Hashtbl.create size in
        List.iter(fun (key, value) -> Hashtbl.add table key value ) init;
        table

    
    let keywords = create_hashtable 10 [
        ("if", IF);
        ("return",RETURN);
        ("else", ELSE);
        ("for",FOR);
        ("while", WHILE);
        ("int",INT);
        ("char", CHAR);
        ("void",VOID);
        ("NULL", NULL);
        ("bool",BOOL);
    ]
}
let digit = ['0' - '9']
let letter = ['a'-'z' 'A'-'Z']
let id = ('_' | letter )('_' | letter | digit)*
let newline = '\r'|'\n'|"\r\n"
let ws = [' ' '\t']
rule token = parse
        | ws+       {token lexbuf}
        | newline+ {Lexing.new_line lexbuf; token lexbuf}
        | id as word {try
                  let kw = Hashtbl.find keywords word in
                  
                  kw
                  with Not_found -> 
                  
                  ID(word)
                 }
        | digit+ as integer { INTEGER(int_of_string integer)}
        | "true" { TRUE}
        | "false"{ FALSE}
        |   (("'")(([' ' -'~' ]) as c)("'")) { CHARLIT(c)}
        | "//" { singlelinecomment lexbuf}
        | "/*" {  multilinecomment lexbuf}
        | '(' { LPAREN}
        | ')' { RPAREN}
        | '{' { LBRACE}
        | '}' { RBRACE}
        | '[' { LBRACKET}
        | ']' { RBRACKET}
        | ';' { SEMI}
        | ',' { COMMA}
        | '+' { PLUS}
        | '-' { MINUS}
        | '*' { TIMES}
        | '/' { DIVIDE}
        | '%' { MOD}
        | '=' { ASSIGN}
        | "==" {EQ}
        | "!=" { NEQ}
        | '<' { LT}
        | '>' { GT}
        | "<=" { LEQ}
        | ">=" { GEQ}
        | '!' { NOT}
        | '&' { ADDRESS}
        | "&&" { AND}
        | "||" { OR}
        | eof   { EOF}
        | _ as c           { Util.raise_lexer_error lexbuf ("Illegal character " ^ Char.escaped c) }

and singlelinecomment = parse
                | newline {Lexing.new_line lexbuf; token lexbuf}
                | _ {singlelinecomment lexbuf}

and multilinecomment = parse
        "*/" {token lexbuf}
        | _ { multilinecomment lexbuf}
