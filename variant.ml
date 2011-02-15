
module M = Id.Map

let rec uniq = function
  | [] -> []
  | x :: xs -> if List.mem x xs then uniq xs else x :: (uniq xs)

module S = Id.Set
  (*i
  struct
    type key = Id.t
    type t = key list
    let empty : t = []
    let rec mem : t -> key -> bool = fun set t ->
      match set with
	| x :: xs -> x = t && mem xs t
	| [] -> false
    let add : key-> t -> t = fun t set ->
      List.rev (uniq (List.rev (t :: set)))
    let elements : t -> key list = fun set ->
      set
  end
  i*)

let empty = M.empty


type tag =
    { tag_number : int; own_variant : Id.t; t: Type.t}
type tags = Id.t * int * tag M.t

let variants : (tag M.t) M.t ref = ref M.empty
let names : S.t ref = ref S.empty


let empty_tags : Id.t -> tags ref =
  fun variantname ->
    ref (variantname, 0, empty)

let add_tag : tags ref -> Id.t -> unit =
  fun x tag ->
    let (vname, num, vmap) = !x in
      x := vname, num + 1, M.add tag {tag_number = num; own_variant = vname; t = Type.Variant vname} vmap

let add_constructor =
  fun x tag ts ->
    let (vname, num, vmap) = !x in
      x := vname, num + 1, M.add tag {tag_number = num; own_variant = vname; t = Type.Fun (ts, Type.Variant vname)} vmap

let add_variant (variantname, num, vmap) =
  variants := M.add variantname vmap !variants;
  names := S.add variantname !names

let for_all_tags f =
  let tags = List.map (fun x-> M.find x !variants) (S.elements !names) in
    f tags

let is_defined id =
  for_all_tags (fun tags ->
		  List.fold_left (&) true (List.rev_map (M.mem id) tags))

let data_of_tag tag =
  for_all_tags (fun tags ->
		  match List.filter (M.mem tag) tags with
		    | [] -> invalid_arg (Format.sprintf "undefined tag %s" tag)
		    | variant :: _ -> M.find tag variant)

let tag_to_variant tag =
  (data_of_tag tag).own_variant

let tag_to_number tag =
  (data_of_tag tag).tag_number

let tag_to_datatype tag =
  (data_of_tag tag).t

let variant_to_exprenv name =
  let tags = M.find name !variants in
  let f tag = TypingType.clos_ot (TypingType.oType_of_type tag.t) in
  TypingExpr.ExprEnv (M.of_assoc (M.map f tags))

let variant_to_module name =
  let tags = M.find name !variants in
  let f tag = TypingType.clos_ot (TypingType.oType_of_type tag.t) in
  TypingExpr.ExprEnv (M.of_assoc (M.map f tags))
