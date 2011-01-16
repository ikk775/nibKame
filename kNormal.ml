open MyUtil

module T = Typing
module TE = TypingExpr
module TT = TypingType

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
  | LetFun of fundef * t (* like let rec *)
  | Apply of Id.t * Id.t list
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
  | ExtFunApply of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

type substitution =
  | Substitution of Id.t * Id.t

let gen_var_num = ref 0

let gen_varname () =
  gen_var_num := !gen_var_num + 1;
  (Format.sprintf "$k:%d" !gen_var_num)

let rec gen_varnames n =
  if n > 0
  then gen_varname () :: gen_varnames (n - 1)
  else []

let gen_var () =
  Var (gen_varname ())

let rec gen_vars n =
  if n > 0
  then gen_var () :: gen_vars (n - 1)
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
  | ArrayAlloc (t, v) -> Sexpr.Sexpr [Sexpr.Sident "k:array-alloc"; Type.to_sexpr t; Sexpr.Sident v]
  | ArrayRef (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:array-ref"; Sexpr.Sident v1; Sexpr.Sident v2]
  | ArraySet (v1, v2, v3) -> Sexpr.Sexpr [Sexpr.Sident "k:array-set"; Sexpr.Sident v1; Sexpr.Sident v2; Sexpr.Sident v3]
  | Cons (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:cons"; Sexpr.Sident v1; Sexpr.Sident v2]
  | Car (v) -> Sexpr.Sexpr [Sexpr.Sident "k:car"; Sexpr.Sident v]
  | Cdr (v) -> Sexpr.Sexpr [Sexpr.Sident "k:cdr"; Sexpr.Sident v]
  | FCons (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "k:cons"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FCar (v) -> Sexpr.Sexpr [Sexpr.Sident "k:car"; Sexpr.Sident v]
  | FCdr (v) -> Sexpr.Sexpr [Sexpr.Sident "k:cdr"; Sexpr.Sident v]
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
  | Sexpr.Sexpr [Sexpr.Sident "k:cons"; Sexpr.Sident v1; Sexpr.Sident v2] -> Cons(v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:car"; Sexpr.Sident v] -> Car(v)
  | Sexpr.Sexpr [Sexpr.Sident "k:cdr"; Sexpr.Sident v] -> Cdr(v)
  | Sexpr.Sexpr [Sexpr.Sident "k:fcons"; Sexpr.Sident v1; Sexpr.Sident v2] -> FCons(v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:fcar"; Sexpr.Sident v] -> FCar(v)
  | Sexpr.Sexpr [Sexpr.Sident "k:fcdr"; Sexpr.Sident v] -> FCdr(v)
  | Sexpr.Sexpr [Sexpr.Sident "k:array-alloc"; t; Sexpr.Sident v] -> ArrayAlloc(Type.of_sexpr t, v)
  | Sexpr.Sexpr [Sexpr.Sident "k:array-ref"; Sexpr.Sident v1; Sexpr.Sident v2] -> ArrayRef(v1, v2)
  | Sexpr.Sexpr [Sexpr.Sident "k:array-set"; Sexpr.Sident v1; Sexpr.Sident v2; Sexpr.Sident v3] -> ArraySet(v1, v2, v3)
  | Sexpr.Sexpr [Sexpr.Sident "k:ext-array"; Sexpr.Sident v] -> ExtArray v
  | Sexpr.Sexpr (Sexpr.Sident "k:ext-fun-apply" :: Sexpr.Sident v :: vs) -> ExtFunApply(v, List.map (function Sexpr.Sident x -> x | _ -> invalid_arg "unexpected token.") vs)
  | _ -> invalid_arg "unexpected token."
and fundef_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "k:fundef"; vt; Sexpr.Sexpr args; body] -> 
    {name = vt_of_sexpr vt; args = List.map vt_of_sexpr args; body = of_sexpr body}
  | _ -> invalid_arg "unexpected token."

let rec freevars_set = function
  | Unit | Int _ | Float _ | Char _ | ExtArray _ -> Id.Set.empty
  | Neg(x) | FNeg(x) -> Id.Set.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) -> Id.Set.of_list [x; y]
  | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) -> Id.Set.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfNotEq(x, y, e1, e2) | IfLsEq(x, y, e1, e2) | IfLs(x, y, e1, e2)
  | IfGtEq(x, y, e1, e2) | IfGt(x, y, e1, e2) -> Id.Set.union (Id.Set.union (Id.Set.of_list [x; y]) (freevars_set e1)) (freevars_set e2)
  | Let((x, _), e1, e2) -> Id.Set.union (freevars_set e1) (Id.Set.diff (freevars_set e2) (Id.Set.singleton x))
  | Var(x) -> Id.Set.singleton x
  | LetFun({name = (x, t); args = yts; body = e1}, e2) ->
    Id.Set.diff (Id.Set.union (freevars_set e2) (Id.Set.diff (freevars_set e1) (Id.Set.of_list (List.map fst yts)))) (Id.Set.singleton x)
  | Apply(x, ys) -> Id.Set.of_list (x :: ys)
  | Tuple(xs) -> Id.Set.of_list xs
  | LetTuple(xts, y, e) -> 
    Id.Set.add y (Id.Set.diff (freevars_set e) (Id.Set.of_list (List.map fst xts)))
  | ExtFunApply(_, ys) -> Id.Set.of_list ys
  | ArrayAlloc(t, i) -> Id.Set.singleton i
  | ArraySet(x, i, y) -> Id.Set.of_list [x; i; y]
  | ArrayRef(x, i) -> Id.Set.of_list [x; i]
  | Set(x, y) -> Id.Set.of_list [x; y]
  | Ref x -> Id.Set.singleton x
  | Car(x) | Cdr(x) | FCar(x) | FCdr(x) -> Id.Set.singleton x
  | Cons(x, y) | FCons(x, y) -> Id.Set.of_list [x; y]

let rec freevars e = Id.Set.elements (freevars_set e)

let rec substitute_map sm = function
  | Unit -> Unit
  | Int i -> Int i
  | Float f -> Float f
  | Char c -> Char c
  | Neg v -> (undefined ())
  | Add (v1, v2) -> (undefined ())
  | Sub (v1, v2) -> (undefined ())
  | Mul (v1, v2) -> (undefined ())
  | Div (v1, v2) -> (undefined ())
  | FNeg v -> (undefined ())
  | FAdd (v1, v2) -> (undefined ())
  | FSub (v1, v2) -> (undefined ())
  | FMul (v1, v2) -> (undefined ())
  | FDiv (v1, v2) -> (undefined ())
  | IfEq (v1, v2, e1, e2) -> (undefined ())
  | IfNotEq (v1, v2, e1, e2) -> (undefined ())
  | IfLs (v1, v2, e1, e2) -> (undefined ())
  | IfLsEq (v1, v2, e1, e2) -> (undefined ())
  | IfGt (v1, v2, e1, e2) -> (undefined ())
  | IfGtEq (v1, v2, e1, e2) -> (undefined ())
  | Let (vt, e1, e2) -> (undefined ())
  | Var v -> (undefined ())
  | LetFun (fd, e) -> (undefined ())
  | Apply (v, vs) -> (undefined ())
  | Tuple vs -> (undefined ())
  | LetTuple (vts, v, e) -> (undefined ())
  | Ref (v) -> (undefined ())
  | Set (v1, v2) -> (undefined ())
  | ArrayAlloc (v1, v2) -> (undefined ())
  | ArrayRef (v1, v2) -> (undefined ())
  | ArraySet (v1, v2, v3) -> (undefined ())
  | ExtArray v -> (undefined ())
  | ExtFunApply (v, vs) -> (undefined ())
  | Cons(v1, v2) -> (undefined ())
  | Car(v) -> (undefined ())
  | Cdr(v) -> (undefined ())
  | FCons(v1, v2) -> (undefined ())
  | FCar(v) -> (undefined ())
  | FCdr(v) -> (undefined ())
and fundef_to_sexpr x = (undefined ())

let internal_symbol name t =
  let operator name ts t f =
    let vs = gen_varnames (List.length ts) in
    let v = gen_varname () in
    LetFun ({ name = name, Type.Fun ([Type.Tuple ts], t); args = [v, Type.Tuple ts]; body = LetTuple (List.combine vs ts, v, f vs) }, Var name)
  in
  let int = Type.Int in
  let float = Type.Int in
  let bool = Type.Bool in
  let li = [int] in
  let lf = [float] in
  let lb = [bool] in
  let lii = [int; int] in
  let lff = [float; float] in
  let lb = [bool] in
  let ob = TypingType.O_Constant bool in
  let ol = TypingType.O_Constant (Type.Variant "list") in
  let olf = TypingType.O_Variant(TypingType.O_Constant Type.Float, ol) in
  let ovf = TypingType.O_Vector (TypingType.O_Constant Type.Float) in
  let omii = TypingType.O_Fun (TypingType.O_Constant int, TypingType.O_Constant int) in
  let omtiii = TypingType.O_Fun (TypingType.O_Tuple [TypingType.O_Constant int; TypingType.O_Constant int], TypingType.O_Constant int) in
  let omff = TypingType.O_Fun (TypingType.O_Constant float, TypingType.O_Constant float) in
  let omtfff = TypingType.O_Fun (TypingType.O_Tuple [TypingType.O_Constant float; TypingType.O_Constant float], TypingType.O_Constant float) in
  let omlff = TypingType.O_Fun (olf, TypingType.O_Constant float) in
  let omlflf = TypingType.O_Fun (olf, olf) in
  let omtflflf = TypingType.O_Fun (TypingType.O_Tuple [TypingType.O_Constant float; olf], olf) in
  let fail () = failwith "BUG: numbers of types and variables are mismatched." in
  match name, t with
    | "%neg", t when t = omii -> operator "%neg" li int (function [v] -> Neg (v) | _ -> fail ()), int
    | "%add", t when t = omtiii -> operator "%add" lii int (function [v1; v2] -> Add (v1, v2) | _ -> fail ()), int
    | "%sub", t when t = omtiii -> operator "%sub" lii int (function [v1; v2] -> Sub (v1, v2) | _ -> fail ()), int
    | "%mul", t when t = omtiii -> operator "%mul" lii int (function [v1; v2] -> Mul (v1, v2) | _ -> fail ()), int
    | "%div", t when t = omtiii -> operator "%div" lii int (function [v1; v2] -> Div (v1, v2) | _ -> fail ()), int
    | "%fneg", t when t = omff -> operator "%fneg" lf float (function [v] -> Neg (v) | _ -> fail ()), float
    | "%fadd", t when t = omtfff -> operator "%fadd" lff float (function [v1; v2] -> FAdd (v1, v2) | _ -> fail ()), float
    | "%fsub", t when t = omtfff -> operator "%fsub" lff float (function [v1; v2] -> FSub (v1, v2) | _ -> fail ()), float
    | "%fmul", t when t = omtfff -> operator "%fmul" lff float (function [v1; v2] -> FMul (v1, v2) | _ -> fail ()), float
    | "%fdiv", t when t = omtfff -> operator "%fdiv" lff float (function [v1; v2] -> FDiv (v1, v2) | _ -> fail ()), float
    | "%true", t when t = ob -> Int 1, int
    | "%false", t when t = ob -> Int 0, int
    | "%cons", t when t = omtflflf -> operator "%fcons" [float; Type.List float] (Type.List float) (function [v1; v2] -> FCons (v1, v2) | _ -> fail ()), Type.List float
    | "%car", t when t = omlff -> operator "%fcar" [Type.List float] float (function [v] -> FCar (v) | _ -> fail ()), float
    | "%cdr", t when t = omlflf -> operator "%fcdr" [Type.List float] (Type.List float) (function [v] -> FCdr (v) | _ -> fail ()), Type.List float
    | "%cons", TypingType.O_Fun (TypingType.O_Tuple [te; tl], tl') -> operator "%cons" (List.map TypingType.oType_to_type [te; tl]) (TypingType.oType_to_type tl') (function [v1; v2] -> Cons (v1, v2) | _ -> fail ()), TypingType.oType_to_type tl'
    | "%car", TypingType.O_Fun (TypingType.O_Tuple [tl], te) -> operator "%car" [TypingType.oType_to_type tl] (TypingType.oType_to_type te) (function [v] -> Car (v) | _ -> fail ()), TypingType.oType_to_type te
    | "%cdr", TypingType.O_Fun (TypingType.O_Tuple [tl], tl') -> operator "%cdr" [TypingType.oType_to_type tl] (TypingType.oType_to_type tl') (function [v] -> Cdr (v) | _ -> fail ()), TypingType.oType_to_type tl'
    | "%ref", TypingType.O_Fun (ft, tt) -> operator "%ref" [TypingType.oType_to_type ft] (TypingType.oType_to_type tt) (function [v] -> Ref (v) | _ -> fail ()),  TypingType.oType_to_type tt
    | "%set", TypingType.O_Fun (TypingType.O_Tuple [tr; te], tt) -> operator "%set" (List.map TypingType.oType_to_type [tr; te]) (TypingType.oType_to_type tt) (function [v1; v2] -> Set (v1, v2) | _ -> fail ()), TypingType.oType_to_type tt
    | "%array-ref", TypingType.O_Fun (TypingType.O_Tuple [ta; tind], te) -> operator "%array-ref" (List.map TypingType.oType_to_type [ta; tind]) (TypingType.oType_to_type te) (function [v1; v2] -> ArrayRef (v1, v2) | _ -> fail ()), TypingType.oType_to_type te
    | "%array-set", TypingType.O_Fun (TypingType.O_Tuple [ta; tind; te], tt) -> operator "%array-set" (List.map TypingType.oType_to_type [ta; tind; te]) (TypingType.oType_to_type tt) (function [v1; v2; v3] -> ArraySet (v1, v2, v3) | _ -> fail ()), TypingType.oType_to_type tt
    | "%array-alloc", TypingType.O_Fun (TypingType.O_Tuple [tnum], ((TypingType.O_Variant (te, TypingType.O_Constant (Type.Variant "array"))) as ta)) -> operator "%array-alloc" (List.map TypingType.oType_to_type [tnum]) (TypingType.oType_to_type ta) (function [v] -> ArrayAlloc (TypingType.oType_to_type te, v) | _ -> fail ()), TypingType.oType_to_type ta
    | "%ignore", t -> (undefined ())
    | "map", t -> (undefined ())
    | _ -> invalid_arg "internal_symbol"
      
let is_valid_internal_symbol name t =
  try
    ignore (internal_symbol name t); true
  with
    | Invalid_argument "internal_symbol"-> false

let rec from_typing_result r =
  let rec f env r =
    Debug.dbgprintsexpr (Typing.to_sexpr r);
    match r with
    | Typing.R_Constant (Syntax.Unit, TypingType.O_Constant Type.Unit) -> Unit, Type.Unit
    | Typing.R_Constant (Syntax.Bool b, TypingType.O_Constant Type.Bool) -> Int (if b then 1 else 0), Type.Int
    | Typing.R_Constant (Syntax.Int i, TypingType.O_Constant Type.Int) -> Int i, Type.Int
    | Typing.R_Constant (Syntax.Float x, TypingType.O_Constant Type.Float) -> Float x, Type.Float
    | Typing.R_Constant (Syntax.Char c, TypingType.O_Constant Type.Float) -> Char c, Type.Char
    | Typing.R_Constant (Syntax.ExtFun f, _) -> (undefined ())
    | Typing.R_Constant (_, _) -> failwith "invalid constant type."
    | Typing.R_External (v, t) when v.[0] = '%' -> internal_symbol v t
    | Typing.R_External (v, t) -> failwith "external function is not supported yet."
    | Typing.R_Let ((v, t), e1, e2) ->
      let e1', t1' = f env e1 in
      let e2', t2' = f (Id.Map.add v t1' env) e2 in
      Let ((v, TT.oType_to_type t), e1', e2'), t2'
    | Typing.R_Variable (v, t) -> Var v, TypingType.oType_to_type t
    | Typing.R_Fun((v, t), e) ->
      let bn = gen_varname () in
      let ta = TT.oType_to_type t in
      let e', t' = f (Id.Map.add v ta env) e in
      let tra = TT.oType_to_type (T.result_type r) in
      LetFun ({name = (bn, TT.oType_to_type t); args = [v, ta]; body = e'}, Var bn), tra
    | Typing.R_Fix ((v, t), (T.R_Fun((vf, tf), ef) as ew), tw) ->
      let bn = gen_varname () in
      let t' = TT.oType_to_type t in
      let ew', tw' = f (Id.Map.add v t' env) ew in
      assert (t' = TT.oType_to_type t);
      begin match ew' with
        | LetFun ({args = [vf, ta]; body = e'}, Var _) -> 
            LetFun ({name = (v, t'); args = [vf, ta]; body = e'}, Var bn), t'
        | _ -> invalid_arg "from_typing_result"
      end
    | Typing.R_Fix ((v, t), _, tw) ->
      invalid_arg "from_typing_result"
    | Typing.R_Apply(Typing.R_Variable (v1, t1), Typing.R_Variable (v2, t2)) ->
      Apply (v1, [v2]), TT.oType_to_type t2
    | Typing.R_Apply(Typing.R_Variable (v, t) as rv, e) ->
      let bn = Typing.gen_varname () in
      let t = Typing.result_type e in
      let e' = Typing.R_Let((bn, t), e, Typing.R_Apply (rv, Typing.R_Variable (bn, t))) in
      f env e'
    | Typing.R_Apply(e1, e2) ->
      let bn = Typing.gen_varname () in
      let t1 = Typing.result_type e1 in
      let e' = Typing.R_Let((bn, t1), e1, Typing.R_Apply (Typing.R_Variable (bn, t1), e2)) in
      f env e'
    | Typing.R_Tuple (es, t) when List.for_all (function Typing.R_Variable _ -> true | _ -> false) es ->
      Tuple (List.map Typing.varname es), TT.oType_to_type t
    | Typing.R_Tuple (es, t) ->
      let bns = Typing.gen_varnames (List.length es) in
      let bs = List.map2 (fun x e -> Typing.R_Variable (x, Typing.result_type e)) bns es in
      let e' = List.fold_left2 (fun e' e b ->
        Typing.R_Let ((b, Typing.result_type e), e, e'))
        (Typing.R_Tuple (bs, t)) es bns
      in
      f env e'
    | Typing.R_Vector (es, t) when List.for_all (function Typing.R_Variable _ -> true | _ -> false) es ->
      undefined ()
    | Typing.R_Vector (es, t) ->
      let bns = Typing.gen_varnames (List.length es) in
      let bs = List.map2 (fun x e -> Typing.R_Variable (x, Typing.result_type e)) bns es in
      let e' = List.fold_left2 (fun e' e b ->
        Typing.R_Let ((b, Typing.result_type e), e, e'))
        (Typing.R_Vector (bs, t)) es bns
      in
      f env e'
    | Typing.R_If (Typing.R_Variable (v, t), e2, e3) ->
      let e2', t2' = f env e2 in
      let e3', t3' = f env e3 in
      IfEq (v, "%true", e2', e3'), t2'
    | Typing.R_If (e1, e2, e3) ->
      let bn = Typing.gen_varname () in
      let t1 = Typing.result_type e1 in
      let e' = Typing.R_Let((bn, t1), e1, Typing.R_If (Typing.R_Variable (bn, t1), e2, e3)) in
      f env e'
  in
  f Id.Map.empty r
  

