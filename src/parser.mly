%{
  open Ast

%}

(* Token declarations *)
%token <int> INTEGER
%token <string> IDENTIFIER
%token <string> STRING
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
%right ASSIGN
%left OR
%left AND
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc NOT USTAR UMINUS REF REFMUT
%nonassoc LBRACK (*RBRACK*)
%nonassoc DOT

(* Start symbol *)
%start program
%type <Ast.program> program

%%

program:
  | functions=list(decl) EOF { functions }

%inline
struct_field:
  | id=IDENTIFIER COLON typ=rtype { (id, typ) }

%inline 
decl_struct:
  | STRUCT id=IDENTIFIER LBRACE flist=separated_list(COMMA, struct_field) RBRACE
    { DeclStruct {name=id; fields=flist} }

%inline 
decl_fun:
  | FN id=IDENTIFIER LPAREN params=separated_list(COMMA, param) RPAREN 
    return_typ = option(preceded(ARROW, rtype))
    body=block
    { DeclFun {name=id; params=params; rtype=return_typ; body=body} }

decl:
  | decl_fun | decl_struct { $1 }

%inline 
param:
  | is_mut=option(MUT) name=IDENTIFIER COLON typ=rtype { (Option.is_some is_mut, name, typ) }

rtype:
  | id=IDENTIFIER {TyBasic id}
  | id=IDENTIFIER LT typ=rtype GT { TyTemplate (id, typ) }
  | REFMUT typ=rtype { TyRefMut typ }
  | REF typ=rtype { TyRef typ }

block_content:
  | first=instr rest=block_ending {
      let (instrs, expr_opt) = rest in
      (first :: instrs, expr_opt)
    }
  | last=expr { ([], Some last) }
  | SEMICOLON bc=block_ending { bc }

block_ending:
  | { ([], None) }
  | next=block_content { next }

block:
  | LBRACE RBRACE { ([], None) }
  | LBRACE bc=block_content RBRACE { bc }
  
%inline
let_field:
  | id=IDENTIFIER COLON exp=expr { (id, exp) }

stmt:
  | exp=expr SEMICOLON { InstrExpr exp }
  | LET is_mut=option(MUT) IDENTIFIER ASSIGN exp=expr SEMICOLON { InstrLetExpr (Option.is_some is_mut, exp) }
  | LET is_mut=option(MUT) IDENTIFIER ASSIGN
    id=IDENTIFIER LBRACE fields=separated_list(COMMA, let_field) RBRACE SEMICOLON
    { InstrLetStruct (Option.is_some is_mut, id, fields) }
  | RETURN exp=option(expr) SEMICOLON { InstrReturn exp }

instr:
  | s=stmt { s }
  | WHILE exp=expr body=block { InstrWhile (exp, body) }
  | nif=rif { InstrIf nif }


rif:
  | IF exp=expr then_body=block { IfElse (exp, then_body, None) }
  | IF exp=expr then_body=block ELSE else_body=block { IfElse (exp, then_body, Some else_body) }
  | IF exp=expr then_body=block ELSE nif=rif { IfElif (exp, then_body, nif) }

%inline
binop: 
  | EQ      { OpEQ      }
  | ASSIGN  { OpASSIGN  }
  | NE      { OpNE      }
  | LT      { OpLT      }
  | GT      { OpGT      }
  | LE      { OpLE      }
  | GE      { OpGE      }
  | AND     { OpAND     }
  | OR      { OpOR      }
  | PLUS    { OpPLUS    }
  | MINUS   { OpMINUS   }
  | STAR    { OpSTAR    }
  | SLASH   { OpSLASH   }
  | PERCENT { OpPERCENT }

%inline
unop: 
  | NOT    { OpNOT }
  | REF    { OpREF }
  | REFMUT { OpREFMUT }

expr:
  | i=INTEGER { ExprInt i}
  | TRUE { ExprBool true }
  | FALSE { ExprBool false }
  | id=IDENTIFIER { ExprIdent id }
  | exp_l=expr op=binop exp_r=expr { ExprBinop (exp_l, op, exp_r) }
  | op=unop exp=expr { ExprUnop (op, exp) }
  | MINUS exp=expr %prec UMINUS { ExprUnop (OpMINUS, exp) }
  | STAR exp=expr %prec USTAR { ExprUnop (OpSTAR, exp) }
  | exp=expr DOT id=IDENTIFIER { ExprField (exp, id) }
  | exp=expr DOT len_id=IDENTIFIER LPAREN RPAREN { if len_id = "len" then 
    ExprLen exp else failwith("TODO: Only .len() method is implemented") }
  | exp1=expr LBRACK exp2=expr RBRACK { ExprBrack (exp1, exp2) }
  | id=IDENTIFIER LPAREN args=separated_list(COMMA, expr) RPAREN { ExprCall (id, args)}
  | id=IDENTIFIER NOT LBRACK args=separated_list(COMMA, expr) RBRACK 
    { if id = "vec" then ExprVec args else failwith("TODO: Only vec! bracket macro is implemented") }
  | id=IDENTIFIER NOT LPAREN str=STRING RPAREN 
    { if id = "print" then ExprPrint str else failwith("TODO: Only print! call macro is implemented") }
  | body=block { ExprBlock body }
  | LPAREN exp=expr RPAREN { exp }
