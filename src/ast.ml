

module Lexing = struct
  include Lexing
  
  let pp_position fmt pos =
    Format.fprintf fmt "{ pos_fname = %S; pos_lnum = %d; pos_bol = %d; pos_cnum = %d }"
      pos.pos_fname pos.pos_lnum pos.pos_bol pos.pos_cnum
      
  let show_position pos =
    Format.asprintf "%a" pp_position pos
end

type loc = Lexing.position * Lexing.position [@@deriving show]

type op = OpEQ | OpNE | OpLT | OpGT | OpLE | OpGE | OpAND | OpOR | OpPLUS
  | OpMINUS | OpSTAR | OpSLASH | OpPERCENT | OpNOT | OpREF | OpREFMUT | OpASSIGN
  [@@deriving show]
  
type expr =
  | ExprInt   of int * loc
  | ExprBool  of bool * loc
  | ExprIdent of string * loc
  | ExprBinop of expr * op * expr * loc
  | ExprUnop  of op * expr * loc
  | ExprField of expr * string * loc
  | ExprLen   of expr * loc
  | ExprBrack of expr * expr * loc
  | ExprCall  of string * expr list * loc
  | ExprVec   of expr list * loc
  | ExprPrint of string * loc
  | ExprBlock of block * loc
  [@@deriving show]

and let_field = string * expr * loc [@@deriving show]

and instr = 
  | InstrExpr      of expr * loc
  | InstrLetExpr   of bool * expr * loc
  | InstrLetStruct of bool * string * let_field list * loc
  | InstrWhile     of expr * block * loc
  | InstrReturn    of expr option * loc
  | InstrIf        of instr_if * loc
  [@@deriving show]

and instr_if = 
  | IfElse of expr * block * block option * loc
  | IfElif of expr * block * instr_if * loc
  [@@deriving show]

and ty = 
  | TyBasic    of string * loc
  | TyTemplate of string * ty * loc
  | TyRefMut   of ty * loc
  | TyRef      of ty * loc
  [@@deriving show]

and field = string * ty * loc [@@deriving show]
and param = bool * string * ty * loc [@@deriving show]
and block = instr list * expr option * loc [@@deriving show]

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
  | DeclStruct of struct_decl * loc
  | DeclFun    of fun_decl * loc
  [@@deriving show]

type program = decl list * loc [@@deriving show];;