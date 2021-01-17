{
    open Parser
    open Lexing
    open Easy_logging

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
    let nex_line lexbuf = 
        let pos = lexbuf.lex_curr_p in 
        lexbuf.lex_curr_p <- 
        { pos with pos_bol = lexbuf.lex_curr_pos;
            pos_lnum = pos.pos_lnum + 1
        }

    let logger = Logging.make_logger "Scanner" Debug [Cli Debug];;
}
let digit = ['0' - '9']
let id = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' '0'-'9']*

rule token = parse
    [' ' '\t' '\r' '\n'] { logger#debug "white space" token lexbuf }
    | id as word {try
                  let kw = Hashtbl.find keywords word in
                  logger#debug "Token %s" word;
                  kw
                  with Not_found -> 
                  logger#debug "identifier %s" word;
                  ID(word)
                 }
    | digit+ as integer {logger#debug "number"; INTEGER(int_of_string integer)}
    | "true" {logger#debug "true" TRUE}
    | "false"{logger#debugv"false" FALSE}
    | '\''['a'-'z''A'-'Z''0'-'9'] '\'' as c {logger#debug "single char" CHAR(c)}
    | "//" {logger#debug "singleline comment"singlelinecomment lexbuf}
    | "/*" {logger#debug "multiline comment" multilinecomment lexbuf}
    | '(' {logger#debug "LPAREN"; LPAREN}
    | ')' {logger#debug "RPAREN"; RPAREN}
    | '{' {logger#debug "LBRACE"; LBRACE}
    | '}' {logger#debug "RBRACE"; RBRACE}
    | '[' {logger#debug "LBRACKET"; LBRACKET}
    | ']' {logger#debug "RBRACKET"; RBRACKET}
    | ';' {logger#debug "SEMI"; SEMI}
    | ',' {logger#debug "COMMA"; COMMA}
    | '+' {logger#debug "PLUS"; PLUS}
    | '-' {logger#debug "MINUS"; MINUS}
    | '*' {logger#debug "TIMES"; TIMES}
    | '/' {logger#debug "DIVIDE"; DIVIDE}
    | '%' {logger#debug "MOD"; MOD}
    | '=' {logger#debug "ASSIGN"; ASSIGN}
    | "==" {logger#debug "EQ ";EQ}
    | "!=" {logger#debug "NEQ"; NEQ}
    | '<' {logger#debug "LT"; LT}
    | '>' {logger#debug "GT" GT}
    | "<=" {logger#debug "LEQ"; LEQ}
    | ">=" {logger#debug "GEQ"; GEQ}
    | '!' {logger#debug "NOT"; NOT}
    | '&' {logger#debug "ADDRESS" ; ADDRESS}
    | "&&" {logger#debug "AND"; AND}
    | "||" {logger#debug "OR"; OR}
    | eof   {logger#debug "eof"; EOF}
    | _ as c           { Util.raise_lexer_error lexbuf ("Illegal character " ^ Char.escaped c) }

and singlelinecomment = parse
    "\n" {token lexbuf}
    | _ {comment lexbuf}

and multilinecomment = parse
    "*/" {token lexbuf}
    | _ { comment lexbuf}