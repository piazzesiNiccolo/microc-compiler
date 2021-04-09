 %{
        open Ast
        open Easy_logging 
        open Lexing
        open Util
        open Lexing

        
    
    (* Define here your utility functions *)
    let log = Logging.make_logger  "Parser" Debug [Cli Debug]
    let create nd loc = {loc = loc; node = nd; id=0}


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



topdec:
| v = vardecl SEMI {create (Vardec(fst v, snd v)) $loc}
| t = typ i = ID LPAREN fs=separated_list(COMMA, vardecl) RPAREN b=block 
  {create (Fundecl({typ=t; fname=i; formals=fs; body=b})) $loc}
;

typ:
  | INT {TypI}
  | CHAR {TypC}
  | BOOL {TypB}
  | VOID {TypV}
;
vardecl:
| t = typ v = vardesc {((fst v) t, snd v)}
;

vardesc:
| i = ID  {((fun t -> t), i)} 
| TIMES v = vardesc %prec ADDRESS {((fun t->TypP((fst v) t)) , snd v )}
| LPAREN v = vardesc RPAREN {v}
| v = vardesc LBRACKET n = option(INTEGER) RBRACKET {((fun t -> TypA((fst v) t,n)), snd v) }
;



block:
| LBRACE c=list(stmtordec) RBRACE { create (Block(c)) $loc}
;

stmtordec:
| s = statement {create (Stmt(s)) $loc}
| v = vardecl SEMI {create (Dec(fst v, snd v)) $loc}
;
statement: 
| RETURN e = option(expr) SEMI {create (Return(e)) $loc}
| e = expr SEMI {create (Expr(e)) $loc}
| b = block {b}
| WHILE LPAREN e = expr RPAREN s=statement {create (While(e, s)) $loc}
| FOR LPAREN init = expr SEMI ext_cond = expr SEMI incr=expr RPAREN s=statement
{
 create (Block([create (Stmt(create (Expr(init)) $loc)) $loc;
              create (Stmt(
                  create (While(ext_cond,
                    create (Block([create (Stmt(s)) $loc;create (Stmt(create (Expr(incr))  $loc)) $loc])) $loc)) 
                  $loc)) 
              $loc;
              ])) 
  $loc
}
| IF LPAREN cond=expr RPAREN s=statement ELSE s2 = statement 
  {create (If(cond,s,s2)) $loc}
| IF LPAREN cond=expr RPAREN s=statement  
  {create (If(cond,s,create (Block([])) $loc)) $loc}  
;

expr:
| r = rexpr {r}
| l = lexpr {create (Access(l)) $loc}
;

lexpr:
| i = ID {create (AccVar(i)) $loc}
| LPAREN l = lexpr RPAREN {l}
| TIMES l = lexpr {create (AccDeref(create (Access(l)) $loc)) $loc}
| l=lexpr LBRACKET e = expr RBRACKET { create (AccIndex(l,e)) $loc}
;

rexpr:
| a = aexpr {a}
| i = ID LPAREN p=separated_list(COMMA,expr) RPAREN 
  {create (Call(i,p)) $loc}
| l = lexpr ASSIGN e = expr {create (Assign(l, e)) $loc}
| NOT e=expr {create (UnaryOp(Not, e)) $loc}
| MINUS e=expr {create (UnaryOp(Neg, e)) $loc}
| e=expr b=binOp e2=expr {create (BinaryOp(b,e,e2)) $loc}
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
  {create (ILiteral(i)) $loc}
| c=CHARLIT 
  {create (CLiteral(c)) $loc}
| TRUE 
  {create (BLiteral(true)) $loc}
| FALSE 
  {create (BLiteral(false)) $loc}
| NULL 
  {create (Access(create (AccVar("NULL")) $loc )) $loc}
| LPAREN r=rexpr RPAREN 
  {r}
| ADDRESS l=lexpr 
  {create (Addr(l)) $loc}
