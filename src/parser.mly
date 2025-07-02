%{
 (* open Ast *)
%}

(* Token declarations *)
%token <int> INTEGER
%token <string> IDENTIFIER
%token TRUE FALSE
%token FN LET MUT IF ELSE WHILE RETURN STRUCT
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NE LT LE GT GE
%token AND OR NOT
%token ASSIGN DOT REFMUT REF
%token LPAREN RPAREN LBRACE RBRACE RBRACK LBRACK
%token SEMICOLON COMMA COLON ARROW
%token EOF

(* Precedence and associativity *)
%left OR
%left AND
%left EQ NE
%left LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT
%right UMINUS

(* Start symbol *)
%start program
(* %type <Ast.program> program *)
%type <int> program


%%

program:
  | INTEGER { $1 }