open MyUtil

module T = Typing
module R = Typing
module TE = TypingExpr
module TT = TypingType
module L = LLifting

let knormal_var_cls = "KN"

type comp = Eq | NotEq | Ls | LsEq | Gt | GtEq
type t =
  | Unit
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
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

type topvar = {
  var_name : Id.t * Type.t;
  expr : t
}

type topDecl =
  | FunDecl of fundef
  | VarDecl of topvar

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

let is_same_name_decl x y = match x, y with
  | VarDecl {var_name = n1}, VarDecl {var_name = n2} -> n1 = n2
  | FunDecl {name = n1}, FunDecl {name = n2} -> n1 = n2
  | _, _ -> false

let vt_to_sexpr = function
  | v, t -> Sexpr.Sexpr [Sexpr.Sident v; Type.to_sexpr t]

let vt_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident v; t] -> (v, Type.of_sexpr t)
  | _ -> invalid_arg "unexpected token."

let rec to_sexpr = function
  | Unit -> Sexpr.Sident "k:unit"
  | Nil tlc -> Sexpr.Sexpr [Sexpr.Sident "k:nil"; Type.listCategory_to_sexpr tlc]
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
  | If (Eq, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (NotEq, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-not-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (Ls, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-ls"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (LsEq, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-ls-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (Gt, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-gt"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (GtEq, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:if-gt-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | Let (vt, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "k:let"; vt_to_sexpr vt; to_sexpr e1; to_sexpr e2]
  | Var v -> Sexpr.Sexpr [Sexpr.Sident "k:var"; Sexpr.Sident v]
  | Apply ((v, t), vs) -> Sexpr.Sexpr (Sexpr.Sident "k:apply" :: Sexpr.Sexpr [Sexpr.Sident v; Type.to_sexpr t] :: List.map (fun x -> Sexpr.Sident x) vs)
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
  | ExtFunApply ((v, t), vs) -> Sexpr.Sexpr (Sexpr.Sident "k:ext-fun-apply" :: Sexpr.Sexpr [Sexpr.Sident v; Type.to_sexpr t] :: List.map (fun x -> Sexpr.Sident x) vs)
and fundef_to_sexpr x = Sexpr.Sexpr [Sexpr.Sident "k:fundef"; vt_to_sexpr x.name; Sexpr.Sexpr (List.map vt_to_sexpr x.args); to_sexpr x.body]

let topDecl_to_sexpr = function
  | FunDecl fd ->
    Sexpr.Sexpr [Sexpr.Sident "k:fun-decl"; fundef_to_sexpr fd]
  | VarDecl {var_name=name; expr=expr} ->
    Sexpr.Sexpr [Sexpr.Sident "k:var-decl"; to_sexpr expr]

let topDecls_to_sexpr decls =
  Sexpr.tagged_sexpr "k:decls" (List.map topDecl_to_sexpr decls)

let rec of_sexpr = function
  | Sexpr.Sident "k:unit" -> Unit
  | Sexpr.Sexpr [Sexpr.Sident "k:nil"; tlc] -> Nil (Type.listCategory_of_sexpr tlc)
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
  | Sexpr.Sexpr [Sexpr.Sident "k:if-eq"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> If (Eq, v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-not-eq"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> If (NotEq, v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-ls-eq"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> If (LsEq, v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-ls"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> If (Ls, v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-gt-eq"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> If (GtEq, v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:if-gt"; Sexpr.Sident v1; Sexpr.Sident v2; e1; e2] -> If (Gt, v1, v2, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:let"; vt; e1; e2] -> Let (vt_of_sexpr vt, of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "k:var"; Sexpr.Sident v] -> Var (v)
  | Sexpr.Sexpr [Sexpr.Sident "k:apply"; Sexpr.Sexpr [Sexpr.Sident v; t]; Sexpr.Sexpr vs] -> Apply ((v, Type.of_sexpr t), List.map (function Sexpr.Sident x -> x | _ -> invalid_arg "unexpected token.") vs)
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
  | Sexpr.Sexpr (Sexpr.Sident "k:ext-fun-apply" :: Sexpr.Sexpr [Sexpr.Sident v; t] :: vs) -> ExtFunApply((v, Type.of_sexpr t), List.map (function Sexpr.Sident x -> x | _ -> invalid_arg "unexpected token.") vs)
  | _ -> invalid_arg "unexpected token."
and fundef_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "k:fundef"; vt; Sexpr.Sexpr args; body] -> 
    {name = vt_of_sexpr vt; args = List.map vt_of_sexpr args; body = of_sexpr body}
  | _ -> invalid_arg "unexpected token."

let rec freevars_set = function
  | Unit | Nil _ | Int _ | Float _ | Char _ | ExtArray _ -> Id.Set.empty
  | Neg(x) | FNeg(x) -> Id.Set.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) -> Id.Set.of_list [x; y]
  | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) -> Id.Set.of_list [x; y]
  | If (comp, x, y, e1, e2) -> Id.Set.union (Id.Set.union (Id.Set.of_list [x; y]) (freevars_set e1)) (freevars_set e2)
  | Let((x, _), e1, e2) -> Id.Set.union (freevars_set e1) (Id.Set.diff (freevars_set e2) (Id.Set.singleton x))
  | Var(x) -> Id.Set.singleton x
  | Apply((x, t), ys) -> Id.Set.of_list (x :: ys)
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
  | Nil tlc -> Nil tlc
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
  | If (comp, v1, v2, e1, e2) -> (undefined ())
  | Let (vt, e1, e2) -> (undefined ())
  | Var v -> (undefined ())
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

let internal_operator name t =
  let operator name ts t f = match ts with
    | [] -> 
      let name' = Type.typed_id_to_string knormal_var_cls name t in
      VarDecl { var_name = name', t; expr =  f [] }, name'
    | [tf] -> 
      let v = gen_varname () in
      let t' = Type.Fun ([tf], t) in
      let name' = Type.typed_id_to_string knormal_var_cls name t' in
      FunDecl { name = name', t'; args = [v, tf]; body =  f [v] }, name'
    | _ -> 
      let vs = gen_varnames (List.length ts) in
      let v = gen_varname () in
      let t' = Type.Fun ([Type.Tuple ts], t) in
      let name' = Type.typed_id_to_string knormal_var_cls name t' in
      FunDecl { name = name', t'; args = [v, Type.Tuple ts]; body = LetTuple (List.combine vs ts, v, f vs) }, name'
  in
  let ext_func ts' t' qualifiers prefix name tg =
    let mname = Syntax.mangle qualifiers prefix name tg in
    let ts = List.map TT.oType_to_type ts' in
    let t = TT.oType_to_type t' in
    match ts with
      | [] -> invalid_arg "ext_func"
      | [tf] -> 
        operator mname ts t (fun vs -> ExtFunApply ((mname, Type.Fun ([tf], t)), vs))
      | _ -> 
        operator mname ts t (fun vs -> ExtFunApply ((mname, Type.Fun ([Type.Tuple ts], t)), vs))
  in
  let int = Type.Int in
  let float = Type.Float in
  let bool = Type.Bool in
  let li = [int] in
  let lf = [float] in
  let lii = [int; int] in
  let lff = [float; float] in
  let ob = TypingType.O_Constant bool in
  let ol = TypingType.O_Constant (Type.Variant "list") in
  let olf = TypingType.O_Variant(TypingType.O_Constant Type.Float, ol) in
  let omii = TypingType.O_Fun (TypingType.O_Constant int, TypingType.O_Constant int) in
  let omtiii = TypingType.O_Fun (TypingType.O_Tuple [TypingType.O_Constant int; TypingType.O_Constant int], TypingType.O_Constant int) in
  let omff = TypingType.O_Fun (TypingType.O_Constant float, TypingType.O_Constant float) in
  let omtfff = TypingType.O_Fun (TypingType.O_Tuple [TypingType.O_Constant float; TypingType.O_Constant float], TypingType.O_Constant float) in
  let omlff = TypingType.O_Fun (olf, TypingType.O_Constant float) in
  let omlflf = TypingType.O_Fun (olf, olf) in
  let omtflflf = TypingType.O_Fun (TypingType.O_Tuple [TypingType.O_Constant float; olf], olf) in
  let fail () = failwith "BUG: numbers of types and variables are mismatched." in
  match name, t with
    | "%neg", t when t = omii -> operator "%neg" li int (function [v] -> Neg (v) | _ -> fail ())
    | "%add", t when t = omtiii -> operator "%add" lii int (function [v1; v2] -> Add (v1, v2) | _ -> fail ())
    | "%sub", t when t = omtiii -> operator "%sub" lii int (function [v1; v2] -> Sub (v1, v2) | _ -> fail ())
    | "%mul", t when t = omtiii -> operator "%mul" lii int (function [v1; v2] -> Mul (v1, v2) | _ -> fail ())
    | "%div", t when t = omtiii -> operator "%div" lii int (function [v1; v2] -> Div (v1, v2) | _ -> fail ())
    | "%fneg", t when t = omff -> operator "%fneg" lf float (function [v] -> FNeg (v) | _ -> fail ())
    | "%fadd", t when t = omtfff -> operator "%fadd" lff float (function [v1; v2] -> FAdd (v1, v2) | _ -> fail ())
    | "%fsub", t when t = omtfff -> operator "%fsub" lff float (function [v1; v2] -> FSub (v1, v2) | _ -> fail ())
    | "%fmul", t when t = omtfff -> operator "%fmul" lff float (function [v1; v2] -> FMul (v1, v2) | _ -> fail ())
    | "%fdiv", t when t = omtfff -> operator "%fdiv" lff float (function [v1; v2] -> FDiv (v1, v2) | _ -> fail ())
    | "%true", t when t = ob -> operator "%true" [] int (function [] -> Int 1 | _ -> fail ())
    | "%false", t when t = ob -> operator "%false" [] int (function [] -> Int 0 | _ -> fail ())
    | "%cons", t when t = omtflflf -> operator "%fcons" [float; Type.List float] (Type.List float) (function [v1; v2] -> FCons (v1, v2) | _ -> fail ())
    | "%car", t when t = omlff -> operator "%fcar" [Type.List float] float (function [v] -> FCar (v) | _ -> fail ())
    | "%cdr", t when t = omlflf -> operator "%fcdr" [Type.List float] (Type.List float) (function [v] -> FCdr (v) | _ -> fail ())
    | "%cons", TypingType.O_Fun (TypingType.O_Tuple [te; tl], tl') -> operator "%cons" (List.map TypingType.oType_to_type [te; tl]) (TypingType.oType_to_type tl') (function [v1; v2] -> Cons (v1, v2) | _ -> fail ())
    | "%car", TypingType.O_Fun (tl, te) -> operator "%car" [TypingType.oType_to_type tl] (TypingType.oType_to_type te) (function [v] -> Car (v) | _ -> fail ())
    | "%cdr", TypingType.O_Fun (tl, tl') -> operator "%cdr" [TypingType.oType_to_type tl] (TypingType.oType_to_type tl') (function [v] -> Cdr (v) | _ -> fail ())
    | "%ref", TypingType.O_Fun (ft, tt) -> operator "%ref" [TypingType.oType_to_type ft] (TypingType.oType_to_type tt) (function [v] -> Ref (v) | _ -> fail ())
    | "%set", TypingType.O_Fun (TypingType.O_Tuple [tr; te], tt) -> operator "%set" (List.map TypingType.oType_to_type [tr; te]) (TypingType.oType_to_type tt) (function [v1; v2] -> Set (v1, v2) | _ -> fail ())
    | "%array-ref", TypingType.O_Fun (TypingType.O_Tuple [ta; tind], te) -> operator "%array-ref" (List.map TypingType.oType_to_type [ta; tind]) (TypingType.oType_to_type te) (function [v1; v2] -> ArrayRef (v1, v2) | _ -> fail ())
    | "%array-set", TypingType.O_Fun (TypingType.O_Tuple [ta; tind; te], tt) -> operator "%array-set" (List.map TypingType.oType_to_type [ta; tind; te]) (TypingType.oType_to_type tt) (function [v1; v2; v3] -> ArraySet (v1, v2, v3) | _ -> fail ())
    | "%array-alloc", TypingType.O_Fun (tnum, ((TypingType.O_Variant (te, TypingType.O_Constant (Type.Variant "array"))) as ta)) -> operator "%array-alloc" (List.map TypingType.oType_to_type [tnum]) (TypingType.oType_to_type ta) (function [v] -> ArrayAlloc (TypingType.oType_to_type te, v) | _ -> fail ())
    | "%eq", TypingType.O_Fun (TypingType.O_Tuple [t1; t2], tb)
    | "%not-eq", TypingType.O_Fun (TypingType.O_Tuple [t1; t2], tb)
    | "%ls", TypingType.O_Fun (TypingType.O_Tuple [t1; t2], tb)
    | "%ls-eq", TypingType.O_Fun (TypingType.O_Tuple [t1; t2], tb)
    | "%gt", TypingType.O_Fun (TypingType.O_Tuple [t1; t2], tb)
    | "%gt-eq", TypingType.O_Fun (TypingType.O_Tuple [t1; t2], tb) ->
      let t = TT.oType_to_type t1 in
      let tn = Type.to_string t in
      let comp = match name with
        | "%eq" -> Eq
        | "%not-eq" -> NotEq
        | "%ls" -> Ls
        | "%ls-eq" -> LsEq
        | "%gt" -> Gt
        | "%gt-eq" -> GtEq
        | _ -> failwith "something went wrong." in
      operator (name ^ "_" ^ tn) [t; t] bool (function [v1; v2] -> If (comp, v1, v2, Int 1, Int 0) | _ -> fail ())
    | _ -> Sexpr.failwith_captioned_sexprs "invalid internal operator" ["name", Sexpr.Sident name; "type", TT.oType_to_sexpr t]


let rec from_llifting r = (undefined ())

let rec from_typing_result r =
  let ext_decls = ref [] in
  let add_decl decl = ext_decls := decl :: !ext_decls in
  let applied_type n t = times n TT.dest_type t in
  let rec f env r =
(*i    Debug.dbgprintsexpr (Typing.to_sexpr r); i*)
    match r with
    | R.R_Constant (Syntax.Unit, _) -> Unit, Type.Unit
    | R.R_Constant (Syntax.Nil, (TypingType.O_Variant (TypingType.O_Constant Type.Float, TypingType.O_Constant (Type.Variant "list"))as t)) ->
      Nil Type.List_Float, TypingType.oType_to_type t
    | R.R_Constant (Syntax.Nil, t) ->
      Nil Type.List_Other, TypingType.oType_to_type t
    | R.R_Constant (Syntax.Bool b, TypingType.O_Constant Type.Bool) -> Int (if b then 1 else 0), Type.Int
    | R.R_Constant (Syntax.Int i, TypingType.O_Constant Type.Int) -> Int i, Type.Int
    | R.R_Constant (Syntax.Float x, TypingType.O_Constant Type.Float) -> Float x, Type.Float
    | R.R_Constant (Syntax.Char c, TypingType.O_Constant Type.Float) -> Char c, Type.Char
    | R.R_Constant (Syntax.ExtFun f, _) -> (undefined ())
    | R.R_Constant (_, _) -> failwith "invalid constant type."
    | R.R_External (v, t) when v.[0] = '%' ->
      let decl, name = internal_operator v t in
      add_decl decl; Var v, TypingType.oType_to_type t
    | R.R_External (v, t) -> Sexpr.failwith_captioned_sexprs "external function is not supported yet. but got:" ["name", Sexpr.Sident v; "type", TT.oType_to_sexpr t]
    | R.R_Let ((v, t), e1, e2) ->
      let e1', t1' = f env e1 in
      let e2', t2' = f (Id.Map.add v t1' env) e2 in
      Let ((v, TT.oType_to_type t), e1', e2'), t2'
    | R.R_Variable (v, t) -> Var v, TypingType.oType_to_type t
    | R.R_Apply (R.R_Variable (v, t), R.R_Variable (v', t')) ->
      Apply ((v, TT.oType_to_type t), [v']), TT.oType_to_type (applied_type 1 t)
    | R.R_Apply (R.R_External (v, t), R.R_Variable (v', t')) when v.[0] = '%' ->
      let decl, name = internal_operator v t in
      add_decl decl; 
      f env (R.R_Apply (R.R_Variable (name, t), R.R_Variable (v', t')))
    | R.R_Apply (R.R_External (v, t), R.R_Variable (v', t')) ->
      ExtFunApply ((v, TT.oType_to_type t), [v']), TT.oType_to_type (applied_type 1 t)
    | R.R_Apply (R.R_Variable _ as vf, arg)
    | R.R_Apply (R.R_External _ as vf, arg) ->
      let bn = R.gen_varname () in
      let t = R.result_type arg in
      f env (R.R_Let ((bn, t), arg, (R.R_Apply (vf, R.R_Variable (bn, t)))))
    | R.R_Apply (lf, args) ->
      let bn = R.gen_varname () in
      let t = R.result_type lf in
      f env (R.R_Let ((bn, t), lf, R.R_Apply (R.R_Variable (bn, t), args)))
    | R.R_Tuple (es, t) when List.for_all (function R.R_Variable _ -> true | _ -> false) es ->
      Tuple (List.map R.varname es), TT.oType_to_type t
    | R.R_Tuple (es, t) ->
      let bns = R.gen_varnames (List.length es) in
      let bs = List.map2 (fun x e -> R.R_Variable (x, R.result_type e)) bns es in
      let e' = List.fold_left2 (fun e' e b ->
        R.R_Let ((b, R.result_type e), e, e'))
        (R.R_Tuple (bs, t)) es bns
      in
      f env e'
    | R.R_Vector (es, t) when List.for_all (function R.R_Variable _ -> true | _ -> false) es ->
      undefined ()
    | R.R_Vector (es, t) ->
      let bns = R.gen_varnames (List.length es) in
      let bs = List.map2 (fun x e -> R.R_Variable (x, R.result_type e)) bns es in
      let e' = List.fold_left2 (fun e' e b ->
        R.R_Let ((b, R.result_type e), e, e'))
        (R.R_Vector (bs, t)) es bns
      in
      f env e'
    | R.R_If (R.R_Variable (v, t), e2, e3) ->
      let e2', t2' = f env e2 in
      let e3', t3' = f env e3 in
      let decl, name = internal_operator "%false" t in
      add_decl decl; 
      If (NotEq, v, name, e2', e3'), t2'
    | R.R_If (e1, e2, e3) ->
      let bn = R.gen_varname () in
      let t1 = R.result_type e1 in
      let e' = R.R_Let((bn, t1), e1, R.R_If (R.R_Variable (bn, t1), e2, e3)) in
      f env e'
    | R.R_Fix _ -> Sexpr.failwith_sexpr "R_Fix must be eliminated in the lamda lifting phase. but got:" (R.to_sexpr r)
    | R.R_Fun _ -> Sexpr.failwith_sexpr "R_Fun must be eliminated in the lamda lifting phase. but got:" (R.to_sexpr r)
    | R.R_Match _ -> Sexpr.failwith_sexpr "Match is not supported yet. but got:" (R.to_sexpr r)
(*i    | R.R_Match (R.R_Variable (v, _), [R.R_P_Tuple (pts, _) as ps, None, expr]) when Pattern.is_tuple_normal ps ->
      let g = function
        | Some v, t -> v, TT.oType_to_type t
        | None, t -> R.R_gen_varname (), TT.oType_to_type t in
      let h = function
        | R.R_RP_Variable (ov, t) -> g (ov, t)
        | _ -> failwith "something went wrong." in
      LetTuple (List.map h pts, v, fst (f env expr)), TT.oType_to_type (R.R_get_type expr)
    | R.R_Match (e, cls) when (match e with R.R_Variable _ -> false | _ -> true) ->
      let te = R.R_get_type e in
      let b = R.R_gen_var te in
      let bn = R.R_varname b in
      f env (R.R_Let ((bn, te), e, R.R_Match (b, cls)))
      i*)
  in
  let k, t = f Id.Map.empty r in
  k, List.unique ~eq:is_same_name_decl (VarDecl {var_name = ("%true", Type.Int); expr= Int 1} :: !ext_decls)

let typing_result_split r =
	let rec g = function
	  | R.R_Fix(v, r, _) -> r
	  | _ -> r in
	let rec f args = function
		| R.R_Fun (v, r) -> f (v :: args) r
		| r' -> args, r' in
  let args, r' = f [] (g r) in
	List.rev args, r'
	
let from_module_expr_decl = function
  | Module.Expr (name, (qtvs, ts, r)) ->
		let args, r' = typing_result_split r in
		let k, extdecls = from_typing_result r' in
		if args = []
		then VarDecl {var_name=name, TT.oType_to_type (R.result_type r); expr=k}, extdecls
		else FunDecl {name=name, TT.oType_to_type (R.result_type r); args=List.map (function v, t -> v, TT.oType_to_type t) args; body=k}, extdecls
  | Module.Type _ -> undefined ()

let from_module_expr_decls decls =
	let deds = List.map from_module_expr_decl decls in
	let ds, eds = List.split deds in
	let eds' = List.unique ~eq:is_same_name_decl (List.concat eds) in
	ds @ eds'
	
let from_module m =
	from_module_expr_decls (Module.defs_expr m)
	
(*i
let from_ll_decls decls = List.unique ~eq:is_same_name_decl (List.concat (List.map from_ll_decl decls))
i*)
