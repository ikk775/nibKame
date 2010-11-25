type t =
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
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
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let genVarNum = ref 0

let genVar () =
  genVarNum := !genVarNum + 1;
  Var (Format.sprintf "#<kNormal-variable:%d>" !genVarNum)

let rec genVars n =
  if n > 0
  then genVar () :: genVars (n - 1)
  else []
