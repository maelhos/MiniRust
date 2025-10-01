type typ =
  | TypUnit
  | TypI32
  | TypBool
  | TypStruct of string
  | TypVec of typ
  | TypEmptyVec
  | TypRef of typ
  | TypMutRef of typ
[@@deriving show]

let rec ( <= ) (t1 : typ) (t2 : typ) : bool =
  match (t1, t2) with
  | _ when t1 = t2 -> true
  | TypMutRef a, TypRef b
  | TypRef a, TypRef b
  | TypMutRef a, TypMutRef b
  | TypVec a, TypVec b -> a <= b
  | TypEmptyVec, TypVec _ -> true
  | _ -> false

let rec list_cast (tl : typ list) : typ option =
  match tl with
  | h :: [] -> Some h
  | h :: tl -> (
    match list_cast tl with
    | Some t -> if t <= h then Some h else if h <= t then Some t else None
    | None -> None)
  | [] -> failwith "empty list"

let is_ref_typ (t : typ) : bool =
  match t with
  | TypRef _ | TypMutRef _ -> true
  | _ -> false

let rec string_of_typ (t : typ) : string =
  match t with
  | TypUnit -> "()"
  | TypI32 -> "i32"
  | TypBool -> "bool"
  | TypEmptyVec -> "Vec<'a>"
  | TypStruct n -> n
  | TypVec t -> "Vec<" ^ string_of_typ t ^ ">"
  | TypRef t -> "&" ^ string_of_typ t
  | TypMutRef t -> "&mut " ^ string_of_typ t
