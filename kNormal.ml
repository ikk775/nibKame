type t =
  | Unit
  | Int of int
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
  | LetRec of fundef * t
  | Apply of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApply of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let genVarNum = ref 0

let genVar () =
  genVarNum := !genVarNum + 1;
  Var (Format.sprintf "$k:%d" !genVarNum)

let rec genVars n =
  if n > 0
  then genVar () :: genVars (n - 1)
  else []

let vt_to_sexpr = function
  | v, t -> Sexpr.Sexpr [Sexpr.Sident v; Type.to_sexpr t]

let vt_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident v; t] -> (v, Type.of_sexpr t)
  | _ -> invalid_arg "unexpected token."

let rec to_sexpr = function
  | Unit -> Sexpr.Sident "k:unit"
  | Int i -> Sexpr.Sint i
  | Float f -> Sexpr.Sfloat f
  | Neg v -> Sexpr.Sexpr [Sexpr.Sident "k:neg"; Sexpr.Sident v]
  | Add (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:add"; Sexpr.Sident v1; Sexpr.Sident v2]
  | Sub (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:sub"; Sexpr.Sident v1; Sexpr.Sident v2]
  | Mul (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:mul"; Sexpr.Sident v1; Sexpr.Sident v2]
  | Div (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:div"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FNeg v -> Sexpr.Sexpr [Sexpr.Sident "k:fneg"; Sexpr.Sident v]
  | FAdd (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:fadd"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FSub (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:fsub"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FMul (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:fmul"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FDiv (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:fdiv"; Sexpr.Sident v1; Sexpr.Sident v2]
  | IfEq (v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | IfNotEq (v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-not-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | IfLs (v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-ls"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | IfLsEq (v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-ls-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | IfGt (v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-gt"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | IfGtEq (v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-gt-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | Let (vt, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:let"; vt_to_sexpr vt; to_sexpr e1; to_sexpr e2]
  | Var v -> Sexpr.Sexpr [Sexpr.Sident "k:var"; Sexpr.Sident v]
  | LetRec (fd, e) -> Sexpr.Sexpr [Sexpr.Sident "k:letrec"; fundef_to_sexpr fd; to_sexpr e]
  | Apply (v, vs) -> Sexpr.Sexpr (Sexpr.Sident "k:apply" :: Sexpr.Sident v :: List.map (fun x -> Sexpr.Sident x) vs)
  | Tuple vs -> Sexpr.Sexpr (Sexpr.Sident "k:tuple" :: List.map (fun x -> Sexpr.Sident x) vs)
  | LetTuple (vts, v, e) ->
    Sexpr.Sexpr [Sexpr.Sident "k:let-tuple"; Sexpr.Sexpr (List.map vt_to_sexpr vts); Sexpr.Sident v; to_sexpr e]
  | Get (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:array-ref"; Sexpr.Sident v1; Sexpr.Sident v2]
  | Put (v1, v2, v3) -> Sexpr.Sexpr [Sexpr.Sident "k:array-set"; Sexpr.Sident v1; Sexpr.Sident v2; Sexpr.Sident v3]
  | ExtArray v -> Sexpr.Sexpr [Sexpr.Sident "k:ext-array"; Sexpr.Sident v]
  | ExtFunApply (v, vs) -> Sexpr.Sexpr (Sexpr.Sident "k:ext-fun-apply" :: Sexpr.Sident v :: List.map (fun x -> Sexpr.Sident x) vs)
and fundef_to_sexpr x = Sexpr.Sexpr [Sexpr.Sident "k:fundef"; vt_to_sexpr x.name; Sexpr.Sexpr (List.map vt_to_sexpr x.args); to_sexpr x.body]

let rec of_sexpr = function
  | Sexpr.Sident "k:unit" -> Unit
  | Sexpr.Sint i -> Int i
  | Sexpr.Sfloat f -> Float f
  | Sexpr.Sexpr [Sexpr.Sident "k:neg"; Sexpr.Sident v] -> Neg (v)
  | Sexpr.Sexpr [Sexpr.Sident "k:add"; Sexpr.Sident v1; Sexpr.Sident v2] -> Add (v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:sub"; Sexpr.Sident v1; Sexpr.Sident v2] -> Sub (v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:mul"; Sexpr.Sident v1; Sexpr.Sident v2] -> Mul (v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:div"; Sexpr.Sident v1; Sexpr.Sident v2] -> Div (v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:fneg"; Sexpr.Sident v] -> FNeg (v)
  | Sexpr.Sexpr [Sexpr.Sident "k:fadd"; Sexpr.Sident v1; Sexpr.Sident v2] -> FAdd (v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:fsub"; Sexpr.Sident v1; Sexpr.Sident v2] -> FSub (v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:fmul"; Sexpr.Sident v1; Sexpr.Sident v2] -> FMul (v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:fdiv"; Sexpr.Sident v1; Sexpr.Sident v2] -> FDiv (v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-eq"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> IfEq (v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-not-eq"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> IfNotEq (v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-ls-eq"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> IfLsEq (v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-ls"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> IfLs (v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-gt-eq"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> IfGtEq (v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-gt"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> IfGt (v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:let"; vt; e1; e2] -> Let (vt_of_sexpr vt, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:var"; Sexpr.Sident v] -> Var (v)
  | Sexpr.Sexpr [Sexpr.Sident "k:letrec"; fd; e] -> LetRec (fundef_of_sexpr fd, of_sexpr e)
  | Sexpr.Sexpr [Sexpr.Sident "k:apply"; Sexpr.Sident v; Sexpr.Sexpr vs] -> Apply (v, List.map (function Sexpr.Sident x -> x | _ -> invalid_arg "unexpected token.") vs)
  | Sexpr.Sexpr [Sexpr.Sident "k:tuple"; Sexpr.Sexpr vs] -> Tuple (List.map (function Sexpr.Sident x -> x | _ -> invalid_arg "unexpected token.") vs)
  | Sexpr.Sexpr [Sexpr.Sident "k:let-tuple"; Sexpr.Sexpr vts; Sexpr.Sident v; e] -> LetTuple (List.map vt_of_sexpr vts, v, of_sexpr e)
  | Sexpr.Sexpr [Sexpr.Sident "k:array-ref"; Sexpr.Sident v1; Sexpr.Sident v2] -> Get(v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:array-set"; Sexpr.Sident v1; Sexpr.Sident v2; Sexpr.Sident v3] -> Put(v1, v2, v3)
  | Sexpr.Sexpr [Sexpr.Sident "k:ext-array"; Sexpr.Sident v] -> ExtArray v
  | Sexpr.Sexpr (Sexpr.Sident "k:ext-fun-apply" :: Sexpr.Sident v :: vs) -> ExtFunApply(v, List.map (function Sexpr.Sident x -> x | _ -> invalid_arg "unexpected token.") vs)
  | _ -> invalid_arg "unexpected token."
and fundef_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "k:fundef"; vt; Sexpr.Sexpr args; body] -> 
    {name = vt_of_sexpr vt; args = List.map vt_of_sexpr args; body = of_sexpr body}
  | _ -> invalid_arg "unexpected token."
