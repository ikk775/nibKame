type t =
  | Unit
  | Bool
  | Int
  | Float
  | Char
  | Fun of t list * t
  | Tuple of t list
  | Array of t
  | Variant of Id.t
  | Var of t option ref
      
let gentype () = Var(ref None)
