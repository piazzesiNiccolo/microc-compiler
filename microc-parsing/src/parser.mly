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
  | p = list(topdec) EOF     {Prog p}                
;

typ:
  | INT {TypI}
  | CHAR {TypC}
  | BOOL {TypB}
  | VOID {TypV}
;

topdec:
| v = vardecl COMMA {}
| f = fundecl {create Fundecl(f) $loc}
;

vardecl:
| t = typ v = vardesc {}
;

vardesc:
| i = ID  {}
| TIMES v = vardesc {}
| LPAREN v = vardesc RPAREN {}
| v = vardesc LBRACKET n = option(INT) RBRACKET {}
;

fundecl:
| t = typ i = ID LPAREN fs=separated_list(COMMA, vardecl) RPAREN b=block 
  {{typ=t; fname=i; formals=fs; body=b}}
;

block:
| LBRACE c=list(stmtordecl) RBRACE { create Block(c) $loc}
;

stmtordec:
| s = statement {create Stmt(s) $loc}
| v = vardecl COMMA {}
;
statement:
| RETURN e = option(expr) COMMA {create Return(e) $loc}
| e = expr COMMA {create Expr(e) $loc}
| b = block {b}
| WHILE LPAREN e = expr RPAREN b = block {create While(e, b) $loc}
| FOR LPAREN init = expr COMMA ext_cond = expr COMMA incr=expr RPAREN b = block {}
| IF LPAREN cond=expr RPAREN s=statement ELSE s2 = statement 
  {create If(cond,s,s2) $loc}
| IF LPAREN cond=expr RPAREN s=statement  
  {create If(cond,s,Block([])) $loc}  
;

expr:
| r = rexpr {r}
| l = lexpr {create Access(l) $loc}
;

lexpr:
| i = ID {create AccVar(i) $loc}
| LPAREN l = lexpr RPAREN {l}
| TIMES l = lexpr {create AccDeref(create Access(l) $loc) $loc}
| l=lexpr LBRACKET e = expr RBRACKET { create AccIndex(l,e) $loc}
;

rexpr:
| a = aexpr {a}
| i = ID LPAREN p=separated_list(COMMA,expr) RPAREN 
  {create Call(i,p) $loc}
| l = lexpr ASSIGN e = expr {create Assign(l, e) $loc}
| NOT e=expr {create UnaryOp(Not, e) $loc}
| MINUS e=expr {create UnaryOp(Neg, e) $loc}
| e=expr b=binOp e2=expr {create BinaryOp(b,e,e2) $loc}
;
binOp:
| PLUS  {Add}
| MINUS   {Sub}
| TIMES   {Mult}
| MOD   {Mod}
| DIVIDE  {Div}
| AND   {And}
| OR  {Or}
| LT {Less}
| GT {Greater}
| LEQ {Leq}
| GEQ {Geq}
| EQ {Equal}
| NEQ {Neq}
;

aexpr:
| i=INTEGER 
  {create ILiteral(i) $loc}
| c=CHARLIT 
  {create CLiteral(c) $loc}
| TRUE 
  {create BLiteral(true) $loc}
| FALSE 
  {create BLiteral(false) $loc}
| NULL 
  {create Access(create AccVar("NULL") $loc ) $loc}
| LPAREN r=rexpr RPAREN 
  {r}
| ADDRESS l=lexpr 
  {create Addr(l) $loc}