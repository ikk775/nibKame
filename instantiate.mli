type elt = Id.t * (Type.mType list * Typing.result)
type intToExtMap = Id.substitution list
type t = { iem : intToExtMap; defs : elt list; }
type ev_usage = (Id.t * TypingType.oType list) list
type tv_usage = (Id.t * TypingType.oType list) list
type tv_cusage = (Id.t * Type.mType list) list

val empty : 'a list
val add : Id.t * TypingType.oType -> ev_usage -> ev_usage
val of_module : Module.t -> ev_usage
val usage : Module.t -> ev_usage -> tv_usage
val usage_isvalid : tv_usage -> bool
val usage_expand_prototype : tv_usage -> tv_usage
val usage_expand : tv_usage -> tv_usage
val usage_filter : tv_usage -> tv_usage
val expand : Module.t -> tv_usage -> Module.t
val instantiate : Module.t -> Module.t
