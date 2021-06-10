 %{
        open Ast
        open Lexing
        open Util
        open Lexing

        let node nd loc = {loc = loc; node = nd; id=0}
       
         (* utility functions to convert a for to a while *)         
        let for_opt_init e loc =
              match e with
              | Some(x) -> node (Stmt(node (Expr(x)) loc)) loc
              | None -> node (Stmt(node (Block([])) loc)) loc
            
        
        let for_opt_cond e loc =
              match e with
              | Some(x) -> x
              | None -> node (BLiteral(true)) loc
  
        let for_opt_incr e loc =
              match e with
              | Some(x) -> node (Stmt(node (Expr(x)) loc)) loc
              | None -> node (Stmt( node (Block([])) loc)) loc
        

%}

/* Tokens declarations */

%token IF RETURN ELSE FOR WHILE DO INT CHAR VOID NULL BOOL FLOAT STRUCT
%token PLUS MINUS TIMES DIVIDE MOD DOT
%token AND OR EQ NEQ NOT GT LT GEQ LEQ
%token ADDRESS ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token INCREMENT DECREMENT
%token COMMA SEMI
%token SHORTADD SHORTDIV SHORTMIN SHORTMUL SHORTMOD
%token <string>ID
%token <int>INTEGER
%token <float> FLOATLIT
%token <char>CHARLIT
%token <string>STRING
%token TRUE FALSE
%token EOF

/* Precedence and associativity specification */
%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN SHORTADD SHORTDIV SHORTMIN SHORTMUL SHORTMOD
%left OR 
%left AND 
%left EQ NEQ
%nonassoc GT LT GEQ LEQ
%left PLUS MINUS 
%left TIMES DIVIDE MOD
%nonassoc NOT ADDRESS
%left DOT
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
| v = vardecl e = option(preceded(ASSIGN,expr)) SEMI {node (Vardec(fst v, snd v,e)) $loc}
| t = typ i = ID LPAREN fs=separated_list(COMMA, vardecl) RPAREN b=block 
  {node (Fundecl({typ=t; fname=i; formals=fs; body=b})) $loc}
| STRUCT i = ID LBRACE l=list(terminated(vardecl,SEMI)) RBRACE SEMI 
  {node (Structdecl({sname=i; fields=l})) $loc}
;

typ:
  | INT {TypI}
  | FLOAT {TypF}
  | CHAR {TypC}
  | BOOL {TypB}
  | VOID {TypV}
  | STRUCT i = ID {TypS(i)}
;
vardecl:
| t = typ v = vardesc {((fst v) t, snd v)}
;

vardesc:
| i = ID  {((fun t -> t), i)} 
| TIMES v = vardesc %prec ADDRESS {((fun t->fst v (TypP(t))) , snd v )}
| LPAREN v = vardesc RPAREN {v}
| v = vardesc LBRACKET n = option(INTEGER) RBRACKET {((fun t -> fst v (TypA(t,n))), snd v) }
;



block:
| LBRACE c=list(stmtordec) RBRACE { node (Block(c)) $loc}
;

stmtordec:
| s = statement {node (Stmt(s)) $loc}
| v = vardecl e = option(preceded(ASSIGN,expr)) SEMI {node (Dec(fst v, snd v,e)) $loc}
;
statement: 
| RETURN e = option(expr) SEMI {node (Return(e)) $loc}
| e = expr SEMI {node (Expr(e)) $loc}
| b = block {b}
| DO s = statement WHILE LPAREN e = expr RPAREN SEMI {node (DoWhile(e, s)) $loc}
| WHILE LPAREN e = expr RPAREN s=statement {node (While(e, s)) $loc}
| FOR LPAREN init = option(expr) SEMI ext_cond = option(expr) SEMI incr=option(expr) RPAREN s=statement
{
    node (Block([for_opt_init init $loc;
              node (Stmt(
                  node (While(for_opt_cond ext_cond $loc,
                    node (Block([node (Stmt(s)) $loc;for_opt_incr incr $loc])) $loc)) 
                  $loc)) 
              $loc;
              ])) 
    $loc
}
| IF LPAREN cond=expr RPAREN s=statement e=elseblock
  {node (If(cond,s,e)) $loc}
;

elseblock:
  | %prec NOELSE{node (Block([])) $loc}
  | ELSE st=statement {st}
;

expr:
| r = rexpr {r}
| l = lexpr {node (Access(l)) $loc}
;

lexpr:
| i = ID {node (AccVar(i)) $loc}
| LPAREN l = lexpr RPAREN {l}
| TIMES l = lexpr {node (AccDeref(node (Access(l)) $loc)) $loc}
| l=lexpr LBRACKET e = expr RBRACKET { node (AccIndex(l,e)) $loc}
| l = lexpr DOT f=ID {node (AccField(l,f)) $loc}
;

rexpr:
| a = aexpr {a}
| i = ID LPAREN p=separated_list(COMMA,expr) RPAREN 
  {node (Call(i,p)) $loc}
| l = lexpr ASSIGN e = expr {node (Assign(l, e)) $loc}
| u=unaryOp e=expr {node (UnaryOp(u, e)) $loc}
| e=expr b=binOp e2=expr  {node (BinaryOp(b,e,e2)) $loc}
| l=lexpr s=shortOp e = expr {node (Assign(l , node (BinaryOp(s,node (Access(l)) $loc,e)) $loc) ) $loc}
| INCREMENT l = lexpr {node (UnaryOp(PreInc,node (Access(l)) $loc )) $loc}
| DECREMENT l = lexpr {node (UnaryOp(PreDec,node (Access(l)) $loc )) $loc}
| l  = lexpr INCREMENT {node (UnaryOp(PostInc,node (Access(l)) $loc )) $loc}
| l  = lexpr DECREMENT {node (UnaryOp(PostDec,node (Access(l)) $loc )) $loc}
;
(*binop is inline in order to not have shift reduce conflicts*)
%inline binOp:
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

%inline unaryOp:
| NOT {Not}
| MINUS {Neg}

%inline shortOp:
|SHORTADD {Add} 
|SHORTDIV {Div}
|SHORTMIN {Sub}
|SHORTMUL {Mult}
|SHORTMOD {Mod}
;
aexpr:
| i=INTEGER 
  {node (ILiteral(i)) $loc}
| c=CHARLIT 
  {node (CLiteral(c)) $loc}
| f=FLOATLIT
  {node (FLiteral(f)) $loc}
| s=STRING 
  {node (String(s)) $loc}
| TRUE 
  {node (BLiteral(true)) $loc}
| FALSE 
  {node (BLiteral(false)) $loc}
| NULL 
  {node (Null) $loc}
| LPAREN r=rexpr RPAREN 
  {r}
| ADDRESS l=lexpr 
  {node (Addr(l)) $loc}
