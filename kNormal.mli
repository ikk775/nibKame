type comp = Eq | NotEq | Ls | LsEq | Gt | GtEq
type t =
    Unit
  | Nil of Type.listCategory
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
  | If of comp * Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
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
type topvar = {
  var_name : Id.t * Type.t;
  expr : t
}
type topDecl =
  | FunDecl of fundef
  | VarDecl of topvar

type substitution = Substitution of Id.t * Id.t

val gen_varname : string -> string
val gen_varnames : string list -> string list
val gen_var : string -> t
val gen_vars : string list -> t list
val vt_to_sexpr : string * Type.t -> Sexpr.t
val vt_of_sexpr : Sexpr.t -> string * Type.t
val to_sexpr : t -> Sexpr.t
val topDecl_to_sexpr : topDecl -> Sexpr.t
val topDecls_to_sexpr : topDecl list -> Sexpr.t
val of_sexpr : Sexpr.t -> t
val fundef_of_sexpr : Sexpr.t -> fundef
val freevars_set : t -> Id.Set.t
val freevars : t -> Id.Set.elt list
val substitute_map : 'a -> t -> t
val fundef_to_sexpr : t -> Sexpr.t
val from_typing_result : Typing.result -> (t * topDecl list)
val from_module : Module.t -> topDecl list
(*i
val from_llifting : LLifting.t -> (t * topDecl list)
val from_ll_decl : LLifting.topDecl -> topDecl list
val from_ll_decls : LLifting.topDecl list -> topDecl list
i*)
val internal_operator : string -> TypingType.oType -> (topDecl * Id.t)
(** A internal symbol name must begin with the letter '%'. *)
(** A internal symbol as a function represents a primitive function. *)
(** A primitive function takes one argument and should not return a function to represent a multi-argument function.
    Therefore, if you want to add a internal symbol as a function that takes more than one arguments,
    you try not to apply currying but to have it taking one tuple packing the arguments and returning its result. *)
