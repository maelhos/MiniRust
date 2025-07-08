

type op = OpEQ | OpNE | OpLT | OpGT | OpLE | OpGE | OpAND | OpOR | OpPLUS
  | OpMINUS | OpSTAR | OpSLASH | OpPERCENT | OpNOT | OpREF | OpREFMUT | OpASSIGN
  [@@deriving show]
  
type expr =
  | ExprInt of int
  | ExprBool of bool
  | ExprIdent of string
  | ExprBinop of expr * op * expr
  | ExprUnop of op * expr
  | ExprField of expr * string
  | ExprLen of expr
  | ExprBrack of expr * expr
  | ExprCall of string * expr list
  | ExprVec of expr list
  | ExprPrint of string
  | ExprBlock of block
  [@@deriving show]

and let_field = string * expr [@@deriving show]

and instr = 
  | InstrExpr of expr
  | InstrLetExpr of bool * expr
  | InstrLetStruct of bool * string * let_field list
  | InstrWhile of expr * block
  | InstrReturn of expr option
  | InstrIf of instr_if
  [@@deriving show]

and instr_if = 
  | IfElse of expr * block * block option
  | IfElif of expr * block * instr_if
  [@@deriving show]

and ty = 
  | TyBasic of string
  | TyTemplate of string * ty
  | TyRefMut of ty
  | TyRef of ty
  [@@deriving show]

and field = string * ty [@@deriving show]
and param = bool * string * ty [@@deriving show]
and block = instr list * expr option [@@deriving show]

type struct_decl = {
  name: string;
  fields: field list;
} [@@deriving show]

type fun_decl = {
  name: string;
  params: param list;
  rtype: ty option;
  body: block;
} [@@deriving show]

type decl = 
  | DeclStruct of struct_decl
  | DeclFun of fun_decl
  [@@deriving show]

type program = decl list [@@deriving show];;