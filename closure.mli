type closure = { entry : Id.l; actual_fv : Id.t list; }
type comp = Eq | NotEq | Ls | LsEq | Gt | GtEq
type t =
  | Unit
  | Nil of Type.listCategory
  | Int of int
  | Char of char
  | Float of float
  | Seq of t * t
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
  | MakeCls of (Id.t * Type.t) * closure * t
  | ApplyCls of (Id.t * Type.t) * Id.t list
  | ApplyDir of (Id.l * Type.t) * Id.t list
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
  | ExtArray of Id.l
and fundef = {
  fun_name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
}
type topvar = { var_name : Id.l * Type.t; expr : t; }
type topDecl = FunDecl of fundef | VarDecl of topvar
val topDecls : topDecl list ref
val from_knormal : KNormal.t -> topDecl list
val to_sexpr : t -> Sexpr.t
val topDecl_to_sexpr : topDecl -> Sexpr.t
val topDecls_to_sexpr : topDecl list -> Sexpr.t