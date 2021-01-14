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
%token ADDRESS
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token <string>ID
%token <int>INTEGER
%token <char>CHAR
%token <bool>BOOL
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
