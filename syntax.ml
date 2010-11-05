type lit =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char

type pat =
  | P_Ident of Id.t
  | P_Literal of lit
  | P_Tuple of pat list
  | P_List of pat list
  | P_Array of pat list
  | P_Variant of Id.t * pat

type t =
  | Literal of lit
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
  | LetRec of (Id.t * Type.t) * t * t
  | TopLet of pat * t
  | TopLetRec of (Id.t * Type.t) * t
  | If of t * t * t
  | Variant of Id.t
  | Fun of (Id.t * Type.t) list * t
  | Var of Id.t
  | Apply of t * t list
  | Tuple of t list
  | Array of t list
  | List of t list
  | Match of t * (pat * t) list
