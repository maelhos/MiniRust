open Ast
open Type

exception SemanticError of string * Ast.loc
module Smap = Map.Make (String)

type env = typ Smap.t * typ Smap.t (* variable map, struct map *)

let rec analyse_ty (t: ty) (ev: env) : typ_ty * env =
  match t with
  | TyBasic (s, loc) -> begin
    match s with
    | "i32" -> (((I32, false, false), TypTyBasic s), ev)
    | "str" -> (((Str, false, false), TypTyBasic s), ev)
    | "bool" -> (((Bool, false, false), TypTyBasic s), ev)
    | _ -> raise (SemanticError ("Unknown type qualifier", loc))
    end
  | TyTemplate (s, t, loc) -> let nt, nev = analyse_ty t ev in begin
    match s with
    | "vec" -> let vtyp = Vec (extract_typ (fst nt)) in 
      (((vtyp, false, false), TypTyTemplate (s, nt)), nev)
    | _ -> raise (SemanticError ("Unknown template type (only vec supported)", loc))
    end
  | TyRefMut (t, _) -> let nt, nev = analyse_ty t ev in
    let refmtyp = MutRef (extract_typ (fst nt)) in 
    (((refmtyp, false, false), TypTyRefMut nt), nev)
  | TyRef (t, _) -> let nt, nev = analyse_ty t ev in
    let reftyp = MutRef (extract_typ (fst nt)) in 
    (((reftyp, false, false), TypTyRef nt), nev)

let rec analyse_function (params: param list) (rtyp: ty option) (body: block) (ev: env) : typ_decl = 
  TypDeclStruct {name=name; fields=(List.map2 (fun (nm, _, _) (_, t) -> (fst t, nm, t)) fields typ_ty_lst)}

let analyze_decl (d: decl) (ev: env) : typ_decl * env =
  match d with
  | DeclStruct ({name; fields}, loc) -> begin 
    if Option.is_some (Smap.find_opt name (snd ev)) then
      raise (SemanticError ("Struct with this name already defined", loc))
    else
      let typ_ty_lst = List.map (fun (n, t, _) -> (n, fst (analyse_ty t ev))) fields in
      let ty_lst = List.map (fun (n, t) -> (n, extract_typ (fst t))) typ_ty_lst in
      let ty = Struct (name, ty_lst) in
      (
        (
          (ty, false, false), 
          TypDeclStruct {name=name; fields=(List.map2 (fun (nm, _, _) (_, t) -> (fst t, nm, t)) fields typ_ty_lst)}
        ), 
        (fst ev, Smap.add name ty (snd ev))
      )
    end
  | DeclFun ({name; params; rtype; body}, loc) -> begin
    (* We overwrite if name present *)
    let val_ev = fst ev in
    let cmap = if Smap.mem name val_ev then Smap.remove name val_ev else val_ev in
      let t = analyse_function params rtype body ev in
      (t, (Smap.add name t cmap, snd ev))
    end

let rec analyze (p: program) ?(ev: env = (Smap.empty, Smap.empty)) : typ_program * env = 
  match fst p with 
  | h::t -> let de, ev2 = analyze_decl h ev in
            let pe, ev3 = analyze (t, snd p) ~ev:ev2 in (de::pe, ev3)
  | [] -> ([], ev)




