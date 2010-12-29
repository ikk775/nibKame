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

type usingCategory =
  | U_Unit
  | U_Int
  | U_Float
  | U_Char
  | U_Tuple
  | U_Ref
  | U_Variant

type refCategory =
  | R_Unit
  | R_Int
  | R_Float
  | R_Char
  | R_Tuple
  | R_Ref
  | R_Variant

type exprCategory =
  | E_Unit
  | E_Int
  | E_Float
  | E_Char
  | E_Tuple
  | E_Ref
  | E_Variant

val to_uc : t -> usingCategory
val to_rc : t -> refCategory
val to_ec : t -> exprCategory
type mType = C_Unit | C_Int | C_Float | C_Char | C_Tuple | C_Ref | C_Variant
val to_mt : t -> mType
val gentypenum : int ref
val gentype : unit -> t
val mt_equal : t -> t -> bool
val equal : t -> t -> bool
val of_sexpr : Sexpr.t -> t
val to_sexpr : t -> Sexpr.t
