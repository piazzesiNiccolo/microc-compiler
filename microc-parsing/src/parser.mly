/*
* MicroC Parser specification
*/

%{
        open Ast
        open Easy_logging 
        open Lexing
        open Util
        open Lexing

        
    
    (* Define here your utility functions *)
    let log = Logging.make_logger  "Parser" Debug [Cli Debug];;
    let create loc node = {loc = loc; node = node; id=0};;


%}

/* Tokens declarations */

%token IF RETURN ELSE FOR WHILE INT CHAR VOID NULL BOOL
%token PLUS MINUS TIMES DIVIDE MOD 
%token AND OR EQ NEQ NOT GT LT GEQ LEQ
%token ADDRESS ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COMMA SEMI
%token <string>ID
%token <int>INTEGER
%token <char>CHARLIT
%token TRUE FALSE
%token EOF

/* Precedence and associativity specification */
%right ASSIGN
%left OR
%left AND 
%left EQ NEQ
%nonassoc GT LT GEQ LEQ
%left PLUS MINUS TIMES DIVIDE MOD
%nonassoc NOT ADDRESS
%nonassoc LBRACKET


/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */



program:
        |  p = list(topdec) EOF     {Prog d}                
;

typ:
  |INT {TypI}
  |CHAR {TypC}
  |BOOL {TypB}
  |VOID {TypV}
;

topdec:
| t = typ i = ID  LPAREN f = formals RPAREN b=block
  {logger#debug " function declaration\n"; 
    create $loc Fundecl({
    typ = t;
    fname = i;
    formals = f;
    body = b;
  })}
| v = vardec SEMI  {logger#debug "global variable\n"; create $loc v }
;


formals:
| {logger#debug " no argumnts\n"; []}
| f = separated_list(COMMA, vardec) {logger#debug "multiple arguments"; f::fs}
;

block:
LBRACE s = stmtordecl RBRACE {logger#debug " new block \n"; create $loc Block(s)}
;

stmt:
  | RETURN e = expr SEMI {create $loc Some(e)}
  ;


expr:
  | r = rexpr {r}
  | l = lexpr {l}
;

lexpr:
  | i = ID
  | LPAREN e = expr RPAREN
  | TIMES l = lexpr
  | TIMES a = aexpr
  | l = lexpr LBRACKET e = expr RBRACKET
;

rexpr:
  | a = aexpr
  | l = lexpr EQ e = expr 
  | NOT e = expr
  | MINUS e = expr
  | e = expr b = binop e = expr

binop:
    PLUS {Add}
  | MINUS {Sub}
  | TIMES {Mult}
  | DIVIDE {Div}
  | MOD   {Mod}
  | AND {And}
  | OR  {Or}
  | LT {Less}
  | GT {Greater}
  | LEQ {Leq}
  | GEQ {Geq}
  | EQ  {Equal}
  | NEQ {Neq}


