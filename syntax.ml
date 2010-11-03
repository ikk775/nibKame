type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
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
  | Eq of t * t
  | NotEq of t * t
  | LsEq of t * t
  | Ls of t * t
  | Gt of t * t
  | GtEq of t * t
  | Let of (Id.t * Type.t)  * t * t
  | LetRec of (Id.t * Type.t) * t * t
  | If of t * t * t
  | Variant of Id.t
  | Fun of (Id.t * Type.t) list * t
  | Var of Id.t
  | Apply of t * t list
  | Tuple of t list
  | Array of t list
  | List of t list
