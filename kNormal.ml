open MyUtil

type t =
  | Unit
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
  | LetFun of fundef * t (* let rec 相当 *)
  | Apply of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Ref of Id.t
  | Set of Id.t * Id.t
  | ArrayRef of Id.t * Id.t
  | ArraySet of Id.t * Id.t * Id.t
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
  | Char c -> Sexpr.Schar c
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
  | LetFun (fd, e) -> Sexpr.Sexpr [Sexpr.Sident "k:letfun"; fundef_to_sexpr fd; to_sexpr e]
  | Apply (v, vs) -> Sexpr.Sexpr (Sexpr.Sident "k:apply" :: Sexpr.Sident v :: List.map (fun x -> Sexpr.Sident x) vs)
  | Tuple vs -> Sexpr.Sexpr (Sexpr.Sident "k:tuple" :: List.map (fun x -> Sexpr.Sident x) vs)
  | LetTuple (vts, v, e) ->
    Sexpr.Sexpr [Sexpr.Sident "k:let-tuple"; Sexpr.Sexpr (List.map vt_to_sexpr vts); Sexpr.Sident v; to_sexpr e]
  | Ref (v) -> Sexpr.Sexpr [Sexpr.Sident "k:ref"; Sexpr.Sident v]
  | Set (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:set"; Sexpr.Sident v1; Sexpr.Sident v2]
  | ArrayRef (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:array-ref"; Sexpr.Sident v1; Sexpr.Sident v2]
  | ArraySet (v1, v2, v3) -> Sexpr.Sexpr [Sexpr.Sident "k:array-set"; Sexpr.Sident v1; Sexpr.Sident v2; Sexpr.Sident v3]
  | ExtArray v -> Sexpr.Sexpr [Sexpr.Sident "k:ext-array"; Sexpr.Sident v]
  | ExtFunApply (v, vs) -> Sexpr.Sexpr (Sexpr.Sident "k:ext-fun-apply" :: Sexpr.Sident v :: List.map (fun x -> Sexpr.Sident x) vs)
and fundef_to_sexpr x = Sexpr.Sexpr [Sexpr.Sident "k:fundef"; vt_to_sexpr x.name; Sexpr.Sexpr (List.map vt_to_sexpr x.args); to_sexpr x.body]

let rec of_sexpr = function
  | Sexpr.Sident "k:unit" -> Unit
  | Sexpr.Sint i -> Int i
  | Sexpr.Sfloat f -> Float f
  | Sexpr.Schar f -> Char f
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
  | Sexpr.Sexpr [Sexpr.Sident "k:letfun"; fd; e] -> LetFun (fundef_of_sexpr fd, of_sexpr e)
  | Sexpr.Sexpr [Sexpr.Sident "k:apply"; Sexpr.Sident v; Sexpr.Sexpr vs] -> Apply (v, List.map (function Sexpr.Sident x -> x | _ -> invalid_arg "unexpected token.") vs)
  | Sexpr.Sexpr [Sexpr.Sident "k:tuple"; Sexpr.Sexpr vs] -> Tuple (List.map (function Sexpr.Sident x -> x | _ -> invalid_arg "unexpected token.") vs)
  | Sexpr.Sexpr [Sexpr.Sident "k:let-tuple"; Sexpr.Sexpr vts; Sexpr.Sident v; e] -> LetTuple (List.map vt_of_sexpr vts, v, of_sexpr e)
  | Sexpr.Sexpr [Sexpr.Sident "k:ref"; Sexpr.Sident v] -> Ref(v)
  | Sexpr.Sexpr [Sexpr.Sident "k:set"; Sexpr.Sident v1; Sexpr.Sident v2] -> Set(v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:array-ref"; Sexpr.Sident v1; Sexpr.Sident v2] -> ArrayRef(v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:array-set"; Sexpr.Sident v1; Sexpr.Sident v2; Sexpr.Sident v3] -> ArraySet(v1, v2, v3)
  | Sexpr.Sexpr [Sexpr.Sident "k:ext-array"; Sexpr.Sident v] -> ExtArray v
  | Sexpr.Sexpr (Sexpr.Sident "k:ext-fun-apply" :: Sexpr.Sident v :: vs) -> ExtFunApply(v, List.map (function Sexpr.Sident x -> x | _ -> invalid_arg "unexpected token.") vs)
  | _ -> invalid_arg "unexpected token."
and fundef_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "k:fundef"; vt; Sexpr.Sexpr args; body] -> 
    {name = vt_of_sexpr vt; args = List.map vt_of_sexpr args; body = of_sexpr body}
  | _ -> invalid_arg "unexpected token."

let rec freeVars_set = function
  | Unit | Int _ | Float _ | Char _ | ExtArray _ -> Id.Set.empty
  | Neg(x) | FNeg(x) -> Id.Set.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) -> Id.Set.of_list [x; y]
  | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) -> Id.Set.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfNotEq(x, y, e1, e2) | IfLsEq(x, y, e1, e2) | IfLs(x, y, e1, e2)
  | IfGtEq(x, y, e1, e2) | IfGt(x, y, e1, e2) -> Id.Set.union (Id.Set.union (Id.Set.of_list [x; y]) (freeVars_set e1)) (freeVars_set e2)
  | Let((x, _), e1, e2) -> Id.Set.union (freeVars_set e1) (Id.Set.diff (freeVars_set e2) (Id.Set.singleton x))
  | Var(x) -> Id.Set.singleton x
  | LetFun({name = (x, t); args = yts; body = e1}, e2) ->
    Id.Set.diff (Id.Set.union (freeVars_set e2) (Id.Set.diff (freeVars_set e1) (Id.Set.of_list (List.map fst yts)))) (Id.Set.singleton x)
  | Apply(x, ys) -> Id.Set.of_list (x :: ys)
  | Tuple(xs) -> Id.Set.of_list xs
  | LetTuple(xts, y, e) -> 
    Id.Set.add y (Id.Set.diff (freeVars_set e) (Id.Set.of_list (List.map fst xts)))
  | ExtFunApply(_, ys) -> Id.Set.of_list ys
  | ArraySet(x, i, y) -> Id.Set.of_list [x; i; y]
  | ArrayRef(x, i) -> Id.Set.of_list [x; i]
  | Set(x, y) -> Id.Set.of_list [x; y]
  | Ref x -> Id.Set.singleton x

let rec freeVars e = Id.Set.elements (freeVars_set e)

let rec of_typingResult = function
  | Typing.R_Constant (l, t) -> undefined
  | Typing.R_Variable (v, t) -> undefined
  | Typing.R_Fun((v, t), e) -> undefined
  | Typing.R_Apply(e1, e2) -> undefined
  | Typing.R_Tuple (es, t) -> undefined
  | Typing.R_Vector (es, t) -> undefined
  | Typing.R_If (e1, e2, e3) -> undefined
  | Typing.R_Let ((v, t), e1, e2) -> undefined
  | Typing.R_Fix ((v, t), e, t') -> undefined

