open Ast
open Type

type anot = {loc: loc; ty: typ; lval: bool; mut: bool} [@@deriving show]

let default_anot (l : loc) : anot =
  {loc= l; ty= TypUnit; lval= false; mut= false}

let var_anot (l : loc) (t : typ) (m : bool) : anot =
  {loc= l; ty= t; lval= true; mut= m}

let typ_anot (l : loc) (t : typ) : anot =
  {loc= l; ty= t; lval= false; mut= false}

let extract_anot_expr (e : 'a expr) : 'a =
  match e with
  | ExprInt (_, a)
  | ExprBool (_, a)
  | ExprIdent (_, a)
  | ExprField (_, _, a)
  | ExprLen (_, a)
  | ExprBrack (_, _, a)
  | ExprCall (_, _, a)
  | ExprVec (_, a)
  | ExprPrint (_, a)
  | ExprBlock (_, a)
  | ExprUnop (_, _, a)
  | ExprBinop (_, _, _, a) -> a

let extract_anot_let_field ((_, _, a) : 'a let_field) : 'a = a

let extract_anot_instr (i : 'a instr) : 'a =
  match i with
  | InstrExpr (_, a)
  | InstrLetExpr (_, _, _, a)
  | InstrLetStruct (_, _, _, _, a)
  | InstrWhile (_, _, a)
  | InstrReturn (_, a)
  | InstrIf (_, a) -> a

let extract_anot_instr_if (i : 'a instr_if) : 'a =
  match i with
  | IfElse (_, _, _, a) | IfElif (_, _, _, a) -> a

let extract_anot_ty (t : 'a ty) : 'a =
  match t with
  | TyBasic (_, a) | TyTemplate (_, _, a) | TyRefMut (_, a) | TyRef (_, a) -> a

let extract_anot_field ((_, _, a) : 'a field) : 'a = a
let extract_anot_param ((_, _, _, a) : 'a param) : 'a = a
let extract_anot_block ((_, _, a) : 'a block) : 'a = a

let extract_anot_decl (d : 'a decl) : 'a =
  match d with
  | DeclStruct (_, a) | DeclFun (_, a) -> a

let extract_anot_program ((_, a) : 'a program) : 'a = a
