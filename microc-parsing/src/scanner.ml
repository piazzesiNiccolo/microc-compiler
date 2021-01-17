# 1 "scanner.mll"
 
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

# 35 "scanner.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\222\255\223\255\002\000\008\000\002\000\003\000\031\000\
    \033\000\235\255\237\255\238\255\239\255\240\255\241\255\242\255\
    \243\255\244\255\245\255\246\255\247\255\085\000\085\000\160\000\
    \170\000\198\000\017\001\255\255\045\001\120\001\148\001\223\001\
    \251\001\019\000\250\255\248\255\249\255\233\255\232\255\229\255\
    \228\255\225\255\224\255\001\000\254\255\255\255\086\000\254\255\
    \049\000\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\033\000\029\000\025\000\024\000\028\000\
    \021\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\019\000\033\000\002\000\
    \001\000\001\000\001\000\255\255\001\000\001\000\001\000\001\000\
    \001\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \001\000\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\044\000\000\000\000\000\047\000\000\000\
    \255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\027\000\045\000\000\000\027\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \027\000\007\000\000\000\000\000\000\000\009\000\004\000\022\000\
    \020\000\019\000\010\000\012\000\013\000\011\000\041\000\021\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\034\000\014\000\006\000\008\000\005\000\040\000\
    \039\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\016\000\038\000\015\000\037\000\024\000\
    \049\000\024\000\024\000\024\000\024\000\024\000\025\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\026\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\018\000\003\000\017\000\042\000\035\000\
    \048\000\000\000\000\000\000\000\036\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \002\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\024\000\000\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\000\000\030\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \024\000\000\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\028\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\000\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\029\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
    \000\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\000\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \031\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\024\000\000\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\032\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\000\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\043\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\033\000\000\000\000\000\000\000\000\000\005\000\
    \006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\007\000\000\000\008\000\000\000\
    \048\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\021\000\
    \046\000\255\255\255\255\255\255\021\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \000\000\043\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\024\000\255\255\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\025\000\255\255\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\046\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \026\000\255\255\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\028\000\255\255\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\029\000\
    \255\255\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\030\000\255\255\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\031\000\255\255\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\032\000\255\255\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 37 "scanner.mll"
                         ( logger#debug "white space" token lexbuf )
# 276 "scanner.ml"

  | 1 ->
let
# 38 "scanner.mll"
            word
# 282 "scanner.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 38 "scanner.mll"
                 (try
                  let kw = Hashtbl.find keywords word in
                  logger#debug "Token %s" word;
                  kw
                  with Not_found -> 
                  logger#debug "identifier %s" word;
                  ID(word)
                 )
# 293 "scanner.ml"

  | 2 ->
let
# 46 "scanner.mll"
                integer
# 299 "scanner.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 46 "scanner.mll"
                        (logger#debug "number"; INTEGER(int_of_string integer))
# 303 "scanner.ml"

  | 3 ->
# 47 "scanner.mll"
             (logger#debug "true" TRUE)
# 308 "scanner.ml"

  | 4 ->
# 48 "scanner.mll"
             (logger#debugv"false" FALSE)
# 313 "scanner.ml"

  | 5 ->
let
# 49 "scanner.mll"
                                          c
# 319 "scanner.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 3) in
# 49 "scanner.mll"
                                            (logger#debug "single char" CHARLIT(c))
# 323 "scanner.ml"

  | 6 ->
# 50 "scanner.mll"
           (logger#debug "singleline comment"singlelinecomment lexbuf)
# 328 "scanner.ml"

  | 7 ->
# 51 "scanner.mll"
           (logger#debug "multiline comment" multilinecomment lexbuf)
# 333 "scanner.ml"

  | 8 ->
# 52 "scanner.mll"
          (logger#debug "LPAREN"; LPAREN)
# 338 "scanner.ml"

  | 9 ->
# 53 "scanner.mll"
          (logger#debug "RPAREN"; RPAREN)
# 343 "scanner.ml"

  | 10 ->
# 54 "scanner.mll"
          (logger#debug "LBRACE"; LBRACE)
# 348 "scanner.ml"

  | 11 ->
# 55 "scanner.mll"
          (logger#debug "RBRACE"; RBRACE)
# 353 "scanner.ml"

  | 12 ->
# 56 "scanner.mll"
          (logger#debug "LBRACKET"; LBRACKET)
# 358 "scanner.ml"

  | 13 ->
# 57 "scanner.mll"
          (logger#debug "RBRACKET"; RBRACKET)
# 363 "scanner.ml"

  | 14 ->
# 58 "scanner.mll"
          (logger#debug "SEMI"; SEMI)
# 368 "scanner.ml"

  | 15 ->
# 59 "scanner.mll"
          (logger#debug "COMMA"; COMMA)
# 373 "scanner.ml"

  | 16 ->
# 60 "scanner.mll"
          (logger#debug "PLUS"; PLUS)
# 378 "scanner.ml"

  | 17 ->
# 61 "scanner.mll"
          (logger#debug "MINUS"; MINUS)
# 383 "scanner.ml"

  | 18 ->
# 62 "scanner.mll"
          (logger#debug "TIMES"; TIMES)
# 388 "scanner.ml"

  | 19 ->
# 63 "scanner.mll"
          (logger#debug "DIVIDE"; DIVIDE)
# 393 "scanner.ml"

  | 20 ->
# 64 "scanner.mll"
          (logger#debug "MOD"; MOD)
# 398 "scanner.ml"

  | 21 ->
# 65 "scanner.mll"
          (logger#debug "ASSIGN"; ASSIGN)
# 403 "scanner.ml"

  | 22 ->
# 66 "scanner.mll"
           (logger#debug "EQ ";EQ)
# 408 "scanner.ml"

  | 23 ->
# 67 "scanner.mll"
           (logger#debug "NEQ"; NEQ)
# 413 "scanner.ml"

  | 24 ->
# 68 "scanner.mll"
          (logger#debug "LT"; LT)
# 418 "scanner.ml"

  | 25 ->
# 69 "scanner.mll"
          (logger#debug "GT" GT)
# 423 "scanner.ml"

  | 26 ->
# 70 "scanner.mll"
           (logger#debug "LEQ"; LEQ)
# 428 "scanner.ml"

  | 27 ->
# 71 "scanner.mll"
           (logger#debug "GEQ"; GEQ)
# 433 "scanner.ml"

  | 28 ->
# 72 "scanner.mll"
          (logger#debug "NOT"; NOT)
# 438 "scanner.ml"

  | 29 ->
# 73 "scanner.mll"
          (logger#debug "ADDRESS" ; ADDRESS)
# 443 "scanner.ml"

  | 30 ->
# 74 "scanner.mll"
           (logger#debug "AND"; AND)
# 448 "scanner.ml"

  | 31 ->
# 75 "scanner.mll"
           (logger#debug "OR"; OR)
# 453 "scanner.ml"

  | 32 ->
# 76 "scanner.mll"
            (logger#debug "eof"; EOF)
# 458 "scanner.ml"

  | 33 ->
let
# 77 "scanner.mll"
           c
# 464 "scanner.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 77 "scanner.mll"
                       ( Util.raise_lexer_error lexbuf ("Illegal character " ^ Char.escaped c) )
# 468 "scanner.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and singlelinecomment lexbuf =
   __ocaml_lex_singlelinecomment_rec lexbuf 43
and __ocaml_lex_singlelinecomment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 80 "scanner.mll"
         (token lexbuf)
# 480 "scanner.ml"

  | 1 ->
# 81 "scanner.mll"
        (comment lexbuf)
# 485 "scanner.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_singlelinecomment_rec lexbuf __ocaml_lex_state

and multilinecomment lexbuf =
   __ocaml_lex_multilinecomment_rec lexbuf 46
and __ocaml_lex_multilinecomment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 84 "scanner.mll"
         (token lexbuf)
# 497 "scanner.ml"

  | 1 ->
# 85 "scanner.mll"
        ( comment lexbuf)
# 502 "scanner.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_multilinecomment_rec lexbuf __ocaml_lex_state

;;

