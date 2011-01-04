type l = L of string
module Set :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val of_list : elt list -> t
    val add_list : elt list -> t -> t
  end

module Map :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val of_assoc : 'a t -> unit
    val add_list : (key * 'a) list -> 'a t -> 'a t
    val add_list2 : key list -> 'a list -> 'a t -> 'a t
    val compose : 'a t -> 'a t -> 'a t
  end
type substitution = Substitution of string * string

val pp_list : string list -> string
val substitute : substitution list -> string -> string
val compose : substitution list -> substitution list -> substitution list
val compose_substs : substitution list list -> substitution list
val substitution_to_sexpr : substitution -> Sexpr.t
val substitution_of_sexpr : Sexpr.t -> substitution
val substitutions_to_sexpr : substitution list -> Sexpr.t
val substitutions_of_sexpr : Sexpr.t -> substitution list
val is_valid : string -> bool
