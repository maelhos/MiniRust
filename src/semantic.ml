open Ast
open Type
open Anot

exception SemanticError of string * loc

module Smap = Map.Make (String)

type env = {
  vars: anot Smap.t;
  structs: typ Smap.t Smap.t;
  funs: (typ list * typ) Smap.t;
}

let name_taken (name : string) (ev : env) : bool =
  Smap.mem name ev.vars || Smap.mem name ev.structs || Smap.mem name ev.funs

let rec analyse_ty (p : loc ty) (ev : env) : anot ty * typ =
  match p with
  | TyBasic (tname, l) -> (
    match tname with
    | "i32" -> (TyBasic (tname, typ_anot l TypI32), TypI32)
    | "bool" -> (TyBasic (tname, typ_anot l TypBool), TypBool)
    | _ when Smap.mem tname ev.structs ->
        let rt = TypStruct tname in
        (TyBasic (tname, typ_anot l rt), rt)
    | _ -> raise (SemanticError (Printf.sprintf "Unknown type \"%s\"" tname, l))
    )
  | TyRef (t, l) ->
      let at, st = analyse_ty t ev in
      let rt = TypRef st in
      (TyRef (at, typ_anot l rt), rt)
  | TyRefMut (t, l) ->
      let at, st = analyse_ty t ev in
      let rt = TypMutRef st in
      (TyRefMut (at, typ_anot l rt), rt)
  | TyTemplate (tname, t, l) ->
      if tname <> "Vec" then
        raise
          (SemanticError
             ( Printf.sprintf
                 "Unknown template type \"%s\" (only Vec is implemented)" tname,
               l ));
      let at, st = analyse_ty t ev in
      let rt = TypVec st in
      (TyTemplate (tname, at, typ_anot l rt), rt)

let rec analyse_expr (p : loc expr) (ev : env) : anot expr * typ =
  match p with
  | ExprInt (v, l) -> (ExprInt (v, typ_anot l TypI32), TypI32)
  | ExprBool (v, l) -> (ExprBool (v, typ_anot l TypBool), TypBool)
  | ExprIdent (name, l) ->
      if Smap.mem name ev.vars then
        raise
          (SemanticError
             (Printf.sprintf "Unknown variable identifier \"%s\"" name, l));
      let an = Smap.find name ev.vars in
      (ExprIdent (name, an), an.ty)
  | ExprBinop (e1, op, e2, l) -> (
      let ae1, t1 = analyse_expr e1 ev in
      let ae2, t2 = analyse_expr e2 ev in
      match op with
      | OpEQ | OpNE | OpLT | OpLE | OpGT | OpGE ->
          if t1 <> TypI32 || t2 <> TypI32 then
            raise
              (SemanticError
                 ( Printf.sprintf "Operator \"%s\" expect variables of type i32"
                     (string_of_op op),
                   l ));
          (ExprBinop (ae1, op, ae2, typ_anot l TypBool), TypBool)
      | OpPLUS | OpMINUS | OpSTAR | OpSLASH | OpPERCENT ->
          if t1 <> TypI32 || t2 <> TypI32 then
            raise
              (SemanticError
                 ( Printf.sprintf "Operator \"%s\" expect variables of type i32"
                     (string_of_op op),
                   l ));
          (ExprBinop (ae1, op, ae2, typ_anot l TypI32), TypI32)
      | OpAND | OpOR ->
          if t1 <> TypBool || t2 <> TypBool then
            raise
              (SemanticError
                 ( Printf.sprintf
                     "Operator \"%s\" expect variables of type bool"
                     (string_of_op op),
                   l ));
          (ExprBinop (ae1, op, ae2, typ_anot l TypBool), TypBool)
      | _ ->
          raise
            (SemanticError
               ( Printf.sprintf "Operator \"%s\" is not a binary operator"
                   (string_of_op op),
                 l )))
  | ExprUnop (op, e, l) -> (
      let ae, t = analyse_expr e ev in
      match op with
      | OpMINUS ->
          if t <> TypI32 then
            raise
              (SemanticError
                 ( Printf.sprintf "Operator \"%s\" expect variables of type i32"
                     (string_of_op op),
                   l ));
          (ExprUnop (op, ae, typ_anot l TypI32), TypI32)
      | OpNOT ->
          if t <> TypBool then
            raise
              (SemanticError
                 ( Printf.sprintf
                     "Operator \"%s\" expect variables of type bool"
                     (string_of_op op),
                   l ));
          (ExprUnop (op, ae, typ_anot l TypBool), TypBool)
      | OpREF ->
          if not (extract_anot_expr ae).lval then
            raise
              (SemanticError
                 ( Printf.sprintf "Operator \"%s\" expect a l-value"
                     (string_of_op op),
                   l ));
          let rt = TypRef t in
          (ExprUnop (op, ae, typ_anot l rt), rt)
      | OpREFMUT ->
          if not (extract_anot_expr ae).lval then
            raise
              (SemanticError
                 ( Printf.sprintf "Operator \"%s\" expect a l-value"
                     (string_of_op op),
                   l ));
          if not (extract_anot_expr ae).mut then
            raise
              (SemanticError
                 ( Printf.sprintf "Operator \"%s\" expect a mutable value"
                     (string_of_op op),
                   l ));
          let rt = TypMutRef t in
          (ExprUnop (op, ae, typ_anot l rt), rt)
      | OpSTAR -> (
        match t with
        | TypRef tr ->
            let at = var_anot l tr false in
            (ExprUnop (op, ae, at), tr)
        | TypMutRef tr ->
            let at = var_anot l tr true in
            (ExprUnop (op, ae, at), tr)
        | _ ->
            raise
              (SemanticError
                 ( Printf.sprintf "Operator \"%s\" expects a reference type"
                     (string_of_op op),
                   l )))
      | _ ->
          raise
            (SemanticError
               ( Printf.sprintf "Operator \"%s\" is not a unary operator"
                   (string_of_op op),
                 l )))
  | ExprField (e, fname, l) -> (
      let ae, t = analyse_expr e ev in
      match t with
      | TypStruct sname ->
          let struct_mems = Smap.find sname ev.structs in
          if not (Smap.mem sname struct_mems) then
            raise
              (SemanticError
                 ( Printf.sprintf
                     "Struct \"%s\" does not have a field named \"%s\"" sname
                     fname,
                   l ));
          if not (extract_anot_expr ae).mut then
            raise
              (SemanticError
                 ( "Operator \".\" expects a l-valued struct or a struct reference",
                   l ));
          let rt = Smap.find sname struct_mems in
          (ExprField (ae, fname, var_anot l rt (extract_anot_expr ae).mut), rt)
      | TypRef (TypStruct sname) ->
          let struct_mems = Smap.find sname ev.structs in
          if not (Smap.mem sname struct_mems) then
            raise
              (SemanticError
                 ( Printf.sprintf
                     "Struct \"%s\" does not have a field named \"%s\"" sname
                     fname,
                   l ));
          let rt = Smap.find sname struct_mems in
          (ExprField (ae, fname, var_anot l rt false), rt)
      | TypMutRef (TypStruct sname) ->
          let struct_mems = Smap.find sname ev.structs in
          if not (Smap.mem sname struct_mems) then
            raise
              (SemanticError
                 ( Printf.sprintf
                     "Struct \"%s\" does not have a field named \"%s\"" sname
                     fname,
                   l ));
          let rt = Smap.find sname struct_mems in
          (ExprField (ae, fname, var_anot l rt true), rt)
      | _ ->
          raise
            (SemanticError
               ( "Operator \".\" expects either a struct or a reference to a struct",
                 l )))
  | ExprLen (e, l) -> (
      let ae, t = analyse_expr e ev in
      match t with
      | TypVec _ | TypRef (TypVec _) | TypMutRef (TypVec _) ->
          (ExprLen (ae, typ_anot l TypI32), TypI32)
      | _ ->
          raise
            (SemanticError
               ( "Method \".len()\" expects either a Vec<t> or a reference to Vec<t>",
                 l )))
  | ExprBrack (e1, e2, l) -> (
      let ae1, t1 = analyse_expr e1 ev in
      let ae2, t2 = analyse_expr e2 ev in
      if t2 <> TypI32 then
        raise
          (SemanticError ("Array-like type can only be indexed by type i32", l));
      match t1 with
      | TypVec ti ->
          if not (extract_anot_expr ae1).lval then
            raise
              (SemanticError
                 ( "Indexing an array-like requires an l-valued Vec<t> or a reference to Vec<t>",
                   l ));
          (ExprBrack (ae1, ae2, var_anot l ti (extract_anot_expr ae1).mut), ti)
      | TypRef (TypVec ti) -> (ExprBrack (ae1, ae2, var_anot l ti false), ti)
      | TypMutRef (TypVec ti) -> (ExprBrack (ae1, ae2, var_anot l ti true), ti)
      | _ ->
          raise
            (SemanticError
               ( "Indexing an array-like requires an l-valued Vec<t> or a reference to Vec<t>",
                 l )))
  | ExprCall (fname, el, l) ->
      if not (Smap.mem fname ev.funs) then
        raise
          (SemanticError (Printf.sprintf "Undefined function \"%s\"" fname, l));
      let fun_ts, fun_ret = Smap.find fname ev.funs in
      let aels = List.map (fun e -> analyse_expr e ev) el in
      let arg_ts = List.map snd aels in
      if List.length arg_ts <> List.length el then
        raise
          (SemanticError
             ( Printf.sprintf
                 "Function \"%s\" expects %d argument while only %d were provided"
                 fname (List.length arg_ts) (List.length el),
               l ));
      List.iter2
        (fun t1 t2 ->
          if not (t1 <= t2) then
            raise
              (SemanticError
                 ( Printf.sprintf
                     "Function call for \"%s\" expected argument type %s but got %s"
                     fname (string_of_typ t1) (string_of_typ t2),
                   l )))
        arg_ts fun_ts;
      (ExprCall (fname, List.map fst aels, typ_anot l fun_ret), fun_ret)
  | ExprVec (el, l) ->
      let aels = List.map (fun e -> analyse_expr e ev) el in
      let arg_ts = List.map snd aels in
      if List.is_empty arg_ts then
        raise
          (SemanticError
             ("Macro call for vec! expects a non empty list. TODO: inference", l));
      let ti = List.hd arg_ts in
      if not (List.for_all (fun te -> te = ti) arg_ts) then
        raise
          (SemanticError
             ( "Macro call for vec! expects all elements to be of the same type",
               l ));
      let rt = TypVec ti in
      (ExprVec (List.map fst aels, typ_anot l rt), rt)
  | ExprPrint (s, l) -> (ExprPrint (s, default_anot l), TypUnit)
  | ExprBlock (b, l) ->
      let ab, t = analyse_block b ev in
      (ExprBlock (ab, typ_anot l t), t)

and analyse_block (p : loc block) (ev : env) : anot block * typ =
  failwith "todo"
(* let rec aux (l : loc instr list) (e : loc expr option) (ev : env) : anot instr list * typ =
    match 
  let il, eo, l = p in *)

let analyse_decl (p : loc decl) (ev : env) : anot decl * env =
  match p with
  | DeclStruct (sdecl, l) ->
      if name_taken sdecl.name ev then
        raise
          (SemanticError
             (Printf.sprintf "Struct name \"%s\" is already taken" sdecl.name, l));
      let ret_fields =
        List.map (fun (name, ty, l) -> (name, ty, default_anot l)) sdecl.fields
      in
      failwith "todo"
  | DeclFun (fdecl, l) -> failwith "todo"

let analyze_program (p : loc program) : anot program =
  let prog, l = p in
  let rec aux (p : loc decl list) (ev : env) : anot decl list =
    match p with
    | [] -> []
    | h :: t ->
        let nh, nev = analyse_decl h ev in
        nh :: aux t nev
  in
  ( aux prog {vars= Smap.empty; structs= Smap.empty; funs= Smap.empty},
    default_anot l )
