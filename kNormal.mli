type t =
    Unit
  | Int of int
  | Char of char
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Div of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfNotEq of Id.t * Id.t * t * t
  | IfLs of Id.t * Id.t * t * t
  | IfLsEq of Id.t * Id.t * t * t
  | IfGt of Id.t * Id.t * t * t
  | IfGtEq of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetFun of fundef * t
  | Apply of (Id.t * Type.t) * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Ref of Id.t
  | Set of Id.t * Id.t
  | ArrayAlloc of Type.t * Id.t
  | ArrayRef of Id.t * Id.t
  | ArraySet of Id.t * Id.t * Id.t
  | Cons of Id.t * Id.t
  | Car of Id.t
  | Cdr of Id.t
  | FCons of Id.t * Id.t
  | FCar of Id.t
  | FCdr of Id.t
  | ExtArray of Id.t
  | ExtFunApply of (Id.t * Type.t) * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t; }
type substitution = Substitution of Id.t * Id.t

val gen_varname : unit -> string
val gen_varnames : int -> string list
val gen_var : unit -> t
val gen_vars : int -> t list
val vt_to_sexpr : string * Type.t -> Sexpr.t
val vt_of_sexpr : Sexpr.t -> string * Type.t
val to_sexpr : t -> Sexpr.t
val of_sexpr : Sexpr.t -> t
val fundef_of_sexpr : Sexpr.t -> fundef
val freevars_set : t -> Id.Set.t
val freevars : t -> Id.Set.elt list
val substitute_map : 'a -> t -> t
val fundef_to_sexpr : t -> Sexpr.t
val from_typing_result : Typing.result -> (t * Type.t)

val internal_operator : string -> TypingType.oType -> (t * Type.t)
(** A internal symbol name must begin with the letter '%'. *)
(** A internal symbol as a function represents a primitive function. *)
(** A primitive function takes one argument and should not return a function to represent a multi-argument function.
    Therefore, if you want to add a internal symbol as a function that takes more than one arguments,
    you try not to apply currying but to have it taking one tuple packing the arguments and returning its result. *)
