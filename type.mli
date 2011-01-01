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

type usingCategory = (* how to use the datum *)
  | U_Unit
  | U_Bool
  | U_Int
  | U_Float
  | U_Char
  | U_Tuple of usingCategory list
  | U_Fun of usingCategory list * usingCategory
  | U_Ref of usingCategory
  | U_List of usingCategory
  | U_Array of usingCategory
  | U_Variant of Id.t

type refCategory = (* how to point the datum on the memory *)
  | R_Unit
  | R_Int
  | R_Float
  | R_Char
  | R_Tuple
  | R_Ref
  | R_Variant

type exprCategory = (* how to store the datum on the memory *)
  | E_Unit
  | E_Int
  | E_Float
  | E_Char
  | E_List
  | E_FList
  | E_Array
  | E_FArray
  | E_Tuple of refCategory list
  | E_Ref
  | E_Variant

val to_uc : t -> usingCategory
val to_rc : t -> refCategory
val to_ec : t -> exprCategory
type mType = (* OBSOLETED *)
  | C_Unit | C_Int | C_Float | C_Char | C_Tuple | C_Ref | C_Variant
val to_mt : t -> mType
val gentypenum : int ref
val gentype : unit -> t
val mt_equal : t -> t -> bool
val equal : t -> t -> bool
val of_sexpr : Sexpr.t -> t
val to_sexpr : t -> Sexpr.t

val to_string : t -> string
val of_string : string -> t