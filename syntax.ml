type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Fadd of t * t
  | Fsub of t * t
  | Fmul of t * t
  | Fdiv of t * t
  | Eq of t * t
  | NotEq of t * t
  | LsE of t * t
  | 
