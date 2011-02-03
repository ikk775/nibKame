type lit =
    Unit
  | Nil
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | ExtFun of string
type pat =
    P_Ident of Id.t
  | P_Literal of lit
  | P_Tuple of pat list
  | P_List of pat list
  | P_Array of pat list
  | P_Variant of Id.t * pat list
  | Any
  | P_And of pat * pat
  | P_Or of pat * pat
  | P_Not of pat
type t =
    Literal of lit
  | Let of pat * t * t
  | LetSimp of (Id.t * Type.t) * t * t
  | LetRec of (Id.t * Type.t) * t * t
  | TopLet of pat * t
  | TopLetSimp of (Id.t * Type.t) * t
  | TopLetRec of (Id.t * Type.t) * t
  | If of t * t * t
  | Variant of Id.t
  | Fix of (Id.t * Type.t) * t
  | Fun of (Id.t * Type.t) list * t
  | Var of Id.t
  | Apply of t * t list
  | Tuple of t list
  | Array of t list
  | List of t list
  | Match of t * (pat * t option * t) list
val lit_to_sexpr : lit -> Sexpr.t
val lit_of_sexpr : Sexpr.t -> lit
val pat_to_sexpr : pat -> Sexpr.t
val pat_of_sexpr : Sexpr.t -> pat
val of_sexpr : t -> Sexpr.t
val to_sexpr : Sexpr.t -> lit
val varname : t -> Id.t
val mangle : string list -> string -> string -> Type.t -> string
val demangle : char Stream.t -> string list * string * string * Type.t
