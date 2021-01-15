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
%token AND OR EQ NEQ NOT GT LT GEQ LEQ TRUE FALSE
%token REFERENCE ADDRESS ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COMMA SEMI
%token <string>ID
%token <int>INTEGER
%token <char>CHAR
%token TRUE FALSE
%token EOF

/* Precedence and associativity specification */
%right EQ
%left OR
%left and
%left EQ NEQ
%nonassoc GT LT GEQ LEQ
%left PLUS MINUS TIMES DIVIDE MOD
%nonassoc NOT AND 
%nonassoc LBRACKET


/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */

program:
  | t = [topdecl]             {PROG([t])}
  |  EOF                      {Prog([])}
;

topdecl:

typ:
    INT   {TypI}
  | CHAR  {TypC}
  | BOOL  {TypB}
  | VOID  {TypV}

block:
 
stmt: 
    RETURN e = expr SEMI            {Return(Some e)}
  | RETURN SEMI                     {Return(None)}
  | e = expr SEMI                   {e}
  | LBRACE b = block RBRACE         {b} 
  | FOR LPAREN e1 = expr SEMI e2 = expr SEMI e3 = expr RPAREN b = block {}

expr:
    e = rexpr {e}
  | e = lexpr {e}
;


lexpr:
    id = ID                                                 {Access(AccVar(id))}
  | LPAREN e = lexpr RPAREN                                 {e}
  | TIMES e1 = lexpr                                        {Access(Accderef(expr))}
  | TIMES e = aexpr                                         {Access(Accderef(e))}
  | el = lexpr LBRACKET e1=expr RBRACKET                    {Access(AccIndex(AccVar(id), e1))}
;


rexpr:
    a = aexpr                                               {a}
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
  | id = ID LPAREN  

aexpr:
    i = INTEGER                                             {ILiteral(i)}
  | c = CHAR                                                {CLiteral(c)}
  | TRUE                                                    {BLiteral(true)}
  | FALSE                                                   {BLiteral(false)}
  | NULL                                                    {Addr(Accderef(-1))}                                                  {BLiteral(false)}
  | LPAREN r = rexpr RPAREN                                 {r}
  | ADDRESS l = lexpr                                       {Addr(Accderef(l))}