
module M =
  Map.Make (struct type t = Id.t let compare = compare end)
module S =
  Set.Make (struct type t = Id.t let compare = compare end)

let empty = M.empty


type tag =
    { datatype : Type.t option; tag_number : int; own_variant : Id.t}
type tags = Id.t * int * tag M.t

let variants : (tag M.t) M.t ref = ref M.empty
let names : S.t ref = ref S.empty


let empty_tags : Id.t -> tags ref =
  fun variantname ->
    ref (variantname, 0, empty)

let add_tag : tags ref -> Id.t -> Type.t option -> unit =
  fun x tag d_Type ->
    let (vname, num, vmap) = !x in
      x := vname, num + 1, M.add tag {datatype = d_Type; tag_number = num; own_variant = vname} vmap


let add_variant (variantname, num, vmap) =
  variants := M.add variantname vmap !variants;
  names := S.add variantname !names

let data_of_tag tag =
  let tags = List.map (fun x -> M.find x !variants) (S.elements !names) in
  let [variant] = List.filter (M.mem tag) tags in
    M.find tag variant

let tag_to_variant tag =
  (data_of_tag tag).own_variant

let tag_to_number tag =
  (data_of_tag tag).tag_number

let tag_to_datatype tag =
  (data_of_tag tag).datatype
