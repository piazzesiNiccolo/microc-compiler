{
    open Parser

    let hashtable init =
        let table = Hashtbl.create List.length init in
        List.iter(fun (key, value) -> Hashtbl.add table key value ) init;
        table

    
    let keywords = hashtable [
        ("if", IF);
        ("return",RETURN);
        ("else", ELSE);
        ("for",FOR);
        ("while", WHILE);
        ("int",INT);
        ("char", CHAR);
        ("void",VOID;
        ("NULL", NULL);
        ("bool",BOOL);
        
    ]

}
let digit = ['0' - '9']
let id = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' '0'-'9']*

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
    | id as word {try
                  let kw = Hashtbl.find keywords word in
                  kw
                  with Not_found -> ID(word)
                 }
    | digit+ as integer {INTEGER(int_of_string integer)}
    | "true" {TRUE}
    | "false"{FALSE}
    | '\''['a'-'z''A'-'Z''0'-'9']{1} '\'' as c {CHAR(c)}
    | "//" {singlelinecomment lexbuf}
    | "/*" {multilinecomment lexbuf}
    | '(' {LPAREN}
    | ')' {RPAREN}
    | '{' {LBRACE}
    | '}' {RBRACE}
    | '[' {LBRACKET}
    | ']' {RBRACKET}
    | ';' {SEMI}
    | ',' {COMMA}
    | '+' {PLUS}
    | '-' {MINUS}
    | '*' {TIMES}
    | '/' {DIVIDE}
    | '%' {MOD}
    | '=' {ASSIGN}
    | "==" {EQ}
    | "!=" {NEQ}
    | '<' {LT}
    | '>' {GT}
    | "<=" {LEQ}
    | ">=" {GEQ}
    | '!' {NOT}
    | '&' {ADDRESS}
    | "&&" {AND}
    | "||" {OR}
    | eof   {EOF}
    | _ as c           { Util.raise_lexer_error lexbuf ("Illegal character " ^ Char.escaped c) }

and singlelinecomment = parse
    "\n" {token lexbuf}
    | _ {comment lexbuf}

and multilinecomment = parse
    "*/" {token lexbuf}
    | _ { comment lexbuf}