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
  |  EOF                      {Prog([])}
;

expr:
    i = INTEGER                                             {ILiteral(i)}
  | c = CHAR                                                {CLiteral(c)}
  | TRUE                                                    {BLiteral(true)}
  | FALSE                                                   {BLiteral(false)}
  | id = ID                                                 {Access(AccVar(id))}
  | TIMES e1 = expr                                         {Access(Accderef(expr))}
  | id=ID LBRACKET e1=expr RBRACKET                         {Access(AccIndex(AccVar(id), e1))}
  | id = ID ASSIGN e = expr                                 {Assign(AccVar(id), e)}
  | TIMES e1=expr EQ e2 = expr                              {Assign(Accderef(e1), e2)}
  | id = ID LBRACKET e1 = expr RBRACKET ASSIGN e2 = expr    {Assign(AccIndex(AccVar(id), e1), e2)}
  | ADDRESS id = id            {Addr(AccVar(id))}
  | ADDRESS TIMES e = expr    {Addr(Accderef(e))}
  | ADDRESS id=ID LBRACKET e1=expr RBRACKET  {Addr(AccIndex(AccVar(id), e))}
  | e1 = expr PLUS e2 = expr {BinaryOP(Add, e1, e2)}
  | e1 = expr MINUS e2 = expr {BinaryOP(Sub, e1, e2)}
  | e1 = expr TIMES e2 = expr {BinaryOP(Mult, e1, e2)}
  | e1 = expr DIVIDE e2 = expr {BinaryOP(Div, e1, e2)}
  | e1 = expr MOD e2 = expr {BinaryOP(Mod, e1, e2)}
  | e1 = expr EQ e2 = expr {BinaryOP(Equal, e1, e2)}
  | e1 = expr NEQ e2 = expr {BinaryOP(Neq, e1, e2)}
  | e1 = expr LT e2 = expr {BinaryOP(Less, e1, e2)}
  | e1 = expr GT e2 = expr {BinaryOP(Greater, e1, e2)}
  | e1 = expr LEQ e2 = expr {BinaryOP(Leq, e1, e2)}
  | e1 = expr GEQ e2 = expr {BinaryOP(Geq, e1, e2)}
  | e1 = expr AND e2 = expr {BinaryOP(And, e1, e2)}
  | e1 = expr OR e2 = expr {BinaryOP(Or, e1, e2)}
  | e1 = expr PLUS e2 = expr {BinaryOP(Add, e1, e2)}
  | MINUS e = expr {UnaryOp(Neg, e)}
  | NOT e = expr {UnaryOp(Not, e)}
  | id = ID LPAREN  
  | LPAREN e = expr RPAREN {e}