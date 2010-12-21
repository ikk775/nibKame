type t =
    Unit
  | Bool
  | Int
  | Float
  | Char
  | Fun of t list * t
  | Tuple of t list
  | List of t
  | Array of t
  | Ref of t
  | Variant of Id.t
  | Var of Id.t
type mType = C_Unit | C_Int | C_Float | C_Char | C_Tuple | C_Ref | C_Variant
val to_mt : t -> mType
val gentypenum : int ref
val gentype : unit -> t
val mt_equal : t -> t -> bool
val equal : t -> t -> bool
val of_sexpr : Sexpr.t -> t
val to_sexpr : t -> Sexpr.t
