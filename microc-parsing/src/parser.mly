/*
* MicroC Parser specification
*/

%{
    open Ast
    (* Define here your utility functions *)


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

vartyp:
  | INT   {TypI}
  | CHAR  {TypC}
  | BOOL  {TypB}
;
funtyp:
  | t = vartyp  {t}
  | VOID        {TypV}
;

program:
  |  d = list(topdecl)            {PROG(d)}
  |  EOF                          {Prog([])}
;

topdecl:
  | v = varDecl  SEMI {v}
  | f =  funDecl      {f}
;

varDecl:
  | t = vartyp i = ID                                       {Vardec(t, i)}
  | t= vartyp TIMES i = ID                                  {Vardec(TypP(t), i)}
  | t = vartyp LPAREN i = ID RPAREN                         {Vardec(t, i)}
  | t = vartyp i = ID LBRACKET n = option(INTEGER) RBRACKET {Vardec(TypA(t,n), i)}
;

funDecl:
  | t = funtyp i = ID LPAREN p = params RPAREN b = block    {Fundecl({typ=t, fname=i,formals = p, body=b})}       
;
params:
  |                                 {[]}
  | p = varDecl                     {[p]}
  | p = varDecl COMMA ps = params   {p::ps}
;

block: 
  |LBRACE ss = statements RBRACE   {Block(ss)}
;
statements:
  |                           {[]}
  | s = stmt ss = statements  {s::ss}
;

stmt:
  | v = varDecl SEMI                {v}
  | RETURN e = expr SEMI            {Return(Some e)}
  | RETURN SEMI                     {Return(None)}
  | e = expr SEMI                   {e}
  | LBRACE b = block RBRACE         {b} 
  | WHILE LPAREN e = expr RPAREN b = block  {While(e,b)}
  (* ADD FOR *)
  | IF LPAREN e = expr RPAREN s = stmt {If(e,s,Block([]))}
  | IF LPAREN e = expr RPAREN s = stmt ELSE s2 = stmt {If(e,s,s2)}
  
expr:
  | e = rexpr {e}
  | e = lexpr {e}
;


lexpr:
  |  id = ID                                                {Access(AccVar(id))}
  | LPAREN e = lexpr RPAREN                                 {e}
  | TIMES e1 = lexpr                                        {Access(Accderef(e1))}
  | TIMES e = aexpr                                         {Access(Accderef(e))}
  | el = lexpr LBRACKET e1=expr RBRACKET                    {Access(AccIndex(AccVar(id), e1))}
;


rexpr:
  | e = aexpr                                               {e}
  | i = ID LPAREN a = fargs RPAREN                          {Call(i, a)}
  | el = lexpr ASSIGN e = expr                              {Assign(AccVar(id), e)}
  | e1 = expr PLUS e2 = expr                                {BinaryOP(Add, e1, e2)}
  | e1 = expr MINUS e2 = expr                               {BinaryOP(Sub, e1, e2)}
  | e1 = expr TIMES e2 = expr                               {BinaryOP(Mult, e1, e2)}
  | e1 = expr DIVIDE e2 = expr                              {BinaryOP(Div, e1, e2)}
  | e1 = expr MOD e2 = expr                                 {BinaryOP(Mod, e1, e2)}
  | e1 = expr EQ e2 = expr                                  {BinaryOP(Equal, e1, e2)}
  | e1 = expr NEQ e2 = expr                                 {BinaryOP(Neq, e1, e2)}
  | e1 = expr LT e2 = expr                                  {BinaryOP(Less, e1, e2)}
  | e1 = expr GT e2 = expr                                  {BinaryOP(Greater, e1, e2)}
  | e1 = expr LEQ e2 = expr                                 {BinaryOP(Leq, e1, e2)}
  | e1 = expr GEQ e2 = expr                                 {BinaryOP(Geq, e1, e2)}
  | e1 = expr AND e2 = expr                                 {BinaryOP(And, e1, e2)}
  | e1 = expr OR e2 = expr                                  {BinaryOP(Or, e1, e2)}
  | MINUS e = expr                                          {UnaryOp(Neg, e)}
  | NOT e = expr                                            {UnaryOp(Not, e)} 
;

aexpr:
  | i = INTEGER                                             {ILiteral(i)}
  | c = CHARLIT                                             {CLiteral(c)}
  | TRUE                                                    {BLiteral(true)}
  | FALSE                                                   {BLiteral(false)}
  | NULL                                                    {Addr(Accderef(-1))}                                              
  | LPAREN r = rexpr RPAREN                                 {r}
  | ADDRESS l = lexpr                                       {Addr(Accderef(l))}
;

fargs:
  | {[]}
  | e = expr {[e]}
  | e = expr COMMA f = fargs {e::f}
;