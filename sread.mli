val list_of_char : string -> char list
val list_of_Char : string -> Syntax.t list
val list_of_pChar : string -> Syntax.pat list
val read_type : Sexpr.t -> Type.t
val read_typelist : Sexpr.t list -> Type.t
val pattern_of_list : Sexpr.t -> Syntax.pat
val change : Sexpr.t -> Syntax.t
