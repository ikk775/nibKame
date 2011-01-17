val unfold : Variant.tags -> Typing.clause -> Typing.clause list

val unfold_result : Variant.tags -> Typing.result -> Typing.result

val unfold_module : Module.t -> Module.t

val is_tuple_normal : Typing.pattern -> bool