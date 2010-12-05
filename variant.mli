
type tags

val empty_tags : Id.t -> tags ref
val add_tag : tags ref -> Id.t -> Type.t option -> unit
val add_variant : tags -> unit
val is_defined : Id.t -> bool
val tag_to_variant : Id.t -> Id.t
val tag_to_number : Id.t -> int
val tag_to_datatype : Id.t -> Type.t option
