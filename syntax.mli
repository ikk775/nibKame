type lit =
    Unit
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
type t =
    Literal of lit
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Fadd of t * t
  | Fsub of t * t
  | Fmul of t * t
  | Fdiv of t * t
  | Cons of t * t
  | Seq of t * t
  | And of t * t
  | Or of t * t
  | Eq of t * t
  | NotEq of t * t
  | LsEq of t * t
  | Ls of t * t
  | Gt of t * t
  | GtEq of t * t
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
