type typ =
  | TypUnit
  | TypI32
  | TypBool
  | TypStruct of string
  | TypVec of typ
  | TypRef of typ
  | TypMutRef of typ
[@@deriving show]

let ( <= ) (t1 : typ) (t2 : typ) : bool =
  match (t1, t2) with
  | _ when t1 = t2 -> true
  | TypMutRef _, TypRef _ -> true
  | _ -> false

let rec string_of_typ (t : typ) : string =
  match t with
  | TypUnit -> "()"
  | TypI32 -> "i32"
  | TypBool -> "bool"
  | TypStruct n -> n
  | TypVec t -> "Vec<" ^ string_of_typ t ^ ">"
  | TypRef t -> "&" ^ string_of_typ t
  | TypMutRef t -> "&mut" ^ string_of_typ t
