
module K = KNormal

type closure = { entry: Id.l; actual_fv: Id.t list }
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
  | MakeCls of (Id.t * Type.t) * closure * t (* let rec 相当 *)
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
  body : t }

type topvar = {
  var_name : Id.l * Type.t;
  expr : t
}

type topDecl =
  | FunDecl of fundef
  | VarDecl of topvar

let topDecls : topDecl list ref = ref []

let rec fv = function
  | Unit | Nil (_) | Int(_) | Float(_) | Char(_) | ExtArray(_) -> Id.Set.empty
  | Neg(x) | FNeg(x) -> Id.Set.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | ArrayRef(x, y) -> Id.Set.of_list [x; y]
  | If(_, x, y, e1, e2) -> Id.Set.add x (Id.Set.add y (Id.Set.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> Id.Set.union (fv e1) (Id.Set.remove x (fv e2))
  | Seq(e1, e2) -> Id.Set.union (fv e1) (fv e2)
  | Var(x) -> Id.Set.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> Id.Set.remove x (Id.Set.union (Id.Set.of_list ys) (fv e))
  | ApplyCls((x, t), ys) -> Id.Set.of_list (x :: ys)
  | ApplyDir(_, xs) | Tuple(xs) -> Id.Set.of_list xs
  | LetTuple(xts, y, e) -> Id.Set.add y (Id.Set.diff (fv e) (Id.Set.of_list (List.map fst xts)))
  | ArrayAlloc(t, n) -> Id.Set.singleton n
  | ArraySet(x, y, z) -> Id.Set.of_list [x; y; z]
  | Car(x) | Cdr(x) | FCar(x) | FCdr(x) -> Id.Set.singleton x
  | Cons(x, y) | FCons(x, y) -> Id.Set.of_list [x; y]
  | Set(x, y) -> Id.Set.of_list [x; y]
  | Ref(x) -> Id.Set.singleton x

let rec g env known k =
(*  Debug.dbgprintsexpr (KNormal.to_sexpr k); *)
  match k with (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  | KNormal.Unit -> Unit
  | KNormal.Nil(tlc) -> Nil(tlc)
  | KNormal.Int(i) -> Int(i)
  | KNormal.Char(c) -> Char(c)
  | KNormal.Float(d) -> Float(d)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.Mul(x, y) -> Mul(x, y)
  | KNormal.Div(x, y) -> Div(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.If (K.Eq, x, y, e1, e2) -> If(Eq, x, y, g env known e1, g env known e2)
  | KNormal.If (K.NotEq, x, y, e1, e2) -> If(Eq, x, y, g env known e1, g env known e2)
  | KNormal.If (K.Ls, x, y, e1, e2) -> If(LsEq, x, y, g env known e1, g env known e2)
  | KNormal.If (K.LsEq, x, y, e1, e2) -> If(LsEq, x, y, g env known e1, g env known e2)
  | KNormal.If (K.Gt, x, y, e1, e2) -> If(LsEq, x, y, g env known e1, g env known e2)
  | KNormal.If (K.GtEq, x, y, e1, e2) -> If(LsEq, x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (Id.Map.add x t env) known e2)
  | KNormal.Var(x) -> Var(x)
(*  | KNormal.LetFun({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* 関数定義の場合 (caml2html: closure_letrec) *)
      (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
     xに自由変数がない(closureを介さずdirectに呼び出せる)
     と仮定し、knownに追加してe1をクロージャ変換してみる *)
    Debug.dbgprintsexpr (Sexpr.tagged_sexpr x [Type.to_sexpr t]);
      let topdecls_backup = !topDecls in
      let env' = Id.Map.add x t env in
      let known' = Id.Set.add x known in
      let e1' = g (Id.Map.add_list yts env') known' e1 in
      (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
      (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) *)
      let zs = Id.Set.diff (fv e1') (Id.Set.of_list (List.map fst yts)) in
      let known', e1' =
    if Id.Set.is_empty zs then known', e1' else
    (* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
    (Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (Id.Set.elements zs)) x;
     Format.eprintf "function %s cannot be directly applied in fact@." x;
     topDecls := topdecls_backup;
     let e1' = g (Id.Map.add_list yts env') known e1 in
     known, e1') in
      let zs = Id.Set.elements (Id.Set.diff (fv e1') (Id.Set.add x (Id.Set.of_list (List.map fst yts)))) in (* 自由変数のリスト *)
      let zts = List.map (fun z -> (z, Id.Map.find z env')) zs in (* ここで自由変数zの型を引くために引数envが必要 *)
      topDecls := FunDecl { fun_name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !topDecls; (* トップレベル関数を追加 *)
      let e2' = g env' known' e2 in
      if Id.Set.mem x (fv e2') then (* xが変数としてe2'に出現するか *)
    MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* 出現していたら削除しない *)
      else
    (Format.eprintf "eliminating closure(s) %s@." x;
     e2') (* 出現しなければMakeClsを削除 *)
    *)
  | KNormal.Apply((x, t), ys) when Id.Set.mem x known -> (* 関数適用の場合 (caml2html: closure_app) *)
      Format.eprintf "directly applying %s@." x;
      ApplyDir((Id.L(x), t), ys)
  | KNormal.Apply((f, t), xs) -> ApplyCls((f, t), xs)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (Id.Map.add_list xts env) known e)
  | KNormal.Cons(x, y) -> Cons(x, y)
  | KNormal.Cdr(x) -> Cdr(x)
  | KNormal.Car(x) -> Car(x)
  | KNormal.FCons(x, y) -> FCons(x, y)
  | KNormal.FCdr(x) -> FCdr(x)
  | KNormal.FCar(x) -> FCar(x)
  | KNormal.Ref(x) -> Ref(x)
  | KNormal.Set(x, y) -> Set(x, y)
  | KNormal.ArrayAlloc(t, n) -> ArrayAlloc(t, n)
  | KNormal.ArrayRef(x, y) -> ArrayRef(x, y)
  | KNormal.ArraySet(x, y, z) -> ArraySet(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(Id.L(x))
  | KNormal.ExtFunApply((x, t), ys) -> ApplyDir((Id.L("_nibkame_" ^ x), t), ys)

let f e =
  topDecls := [];
  let e' = g Id.Map.empty Id.Set.empty e in
  List.rev !topDecls, e'

let from_knormal k = fst (f k)

let l_to_string = function
  | Id.L str ->  str

let closure_to_sexpr c = Sexpr.tagged_sexpr "closure" [ Sexpr.Sident (l_to_string c.entry); Sexpr.Sexpr ( List.map Sexpr.ident c.actual_fv)]

let vt_to_sexpr = function v, t -> Sexpr.Sexpr [Sexpr.Sident v; Type.to_sexpr t]
let lt_to_sexpr = function Id.L v, t -> Sexpr.Sexpr [Sexpr.Sident v; Type.to_sexpr t]
  
let rec to_sexpr = function
  | Unit -> Sexpr.Sident "c:unit"
  | Nil tlc -> Sexpr.Sexpr [Sexpr.Sident "c:nil"; Type.listCategory_to_sexpr tlc]
  | Int i -> Sexpr.Sint i
  | Float f -> Sexpr.Sfloat f
  | Char c -> Sexpr.Schar c
  | Neg v -> Sexpr.Sexpr [Sexpr.Sident "c:neg"; Sexpr.Sident v]
  | Add (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:add"; Sexpr.Sident v1; Sexpr.Sident v2]
  | Sub (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:sub"; Sexpr.Sident v1; Sexpr.Sident v2]
  | Mul (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:mul"; Sexpr.Sident v1; Sexpr.Sident v2]
  | Div (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:div"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FNeg v -> Sexpr.Sexpr [Sexpr.Sident "c:fneg"; Sexpr.Sident v]
  | FAdd (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:fadd"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FSub (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:fsub"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FMul (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:fmul"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FDiv (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:fdiv"; Sexpr.Sident v1; Sexpr.Sident v2]
  | If (Eq, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "c:if-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (NotEq, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "c:if-not-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (Ls, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "c:if-ls"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (LsEq, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "c:if-ls-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (Gt, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "c:if-gt"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | If (GtEq, v1, v2, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "c:if-gt-eq"; Sexpr.Sident v1; Sexpr.Sident v2; to_sexpr e1; to_sexpr e2]
  | Let (vt, e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "c:let"; vt_to_sexpr vt; to_sexpr e1; to_sexpr e2]
  | Var v -> Sexpr.Sexpr [Sexpr.Sident "c:var"; Sexpr.Sident v]
  | MakeCls (vt, cl, e) -> Sexpr.Sexpr [Sexpr.Sident "c:closure"; vt_to_sexpr vt; closure_to_sexpr cl; to_sexpr e]
  | ApplyCls ((v, t), vs) -> Sexpr.Sexpr (Sexpr.Sident "c:apply-closure" :: Sexpr.Sexpr [Sexpr.Sident v; Type.to_sexpr t] :: List.map (fun x -> Sexpr.Sident x) vs)
  | ApplyDir ((Id.L v, t), vs) -> Sexpr.Sexpr (Sexpr.Sident "c:apply-direct" :: Sexpr.Sexpr [Sexpr.Sident v; Type.to_sexpr t] :: List.map (fun x -> Sexpr.Sident x) vs)
  | Tuple vs -> Sexpr.Sexpr (Sexpr.Sident "c:tuple" :: List.map (fun x -> Sexpr.Sident x) vs)
  | LetTuple (vts, v, e) ->
    Sexpr.Sexpr [Sexpr.Sident "c:let-tuple"; Sexpr.Sexpr (List.map vt_to_sexpr vts); Sexpr.Sident v; to_sexpr e]
  | Ref (v) -> Sexpr.Sexpr [Sexpr.Sident "c:ref"; Sexpr.Sident v]
  | Set (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:set"; Sexpr.Sident v1; Sexpr.Sident v2]
  | ArrayAlloc (t, v) -> Sexpr.Sexpr [Sexpr.Sident "c:array-alloc"; Type.to_sexpr t; Sexpr.Sident v]
  | ArrayRef (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:array-ref"; Sexpr.Sident v1; Sexpr.Sident v2]
  | ArraySet (v1, v2, v3) -> Sexpr.Sexpr [Sexpr.Sident "c:array-set"; Sexpr.Sident v1; Sexpr.Sident v2; Sexpr.Sident v3]
  | Cons (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:cons"; Sexpr.Sident v1; Sexpr.Sident v2]
  | Car (v) -> Sexpr.Sexpr [Sexpr.Sident "c:car"; Sexpr.Sident v]
  | Cdr (v) -> Sexpr.Sexpr [Sexpr.Sident "c:cdr"; Sexpr.Sident v]
  | FCons (v1, v2) -> Sexpr.Sexpr [Sexpr.Sident "c:cons"; Sexpr.Sident v1; Sexpr.Sident v2]
  | FCar (v) -> Sexpr.Sexpr [Sexpr.Sident "c:car"; Sexpr.Sident v]
  | FCdr (v) -> Sexpr.Sexpr [Sexpr.Sident "c:cdr"; Sexpr.Sident v]
  | ExtArray (Id.L v) -> Sexpr.Sexpr [Sexpr.Sident "c:ext-array"; Sexpr.Sident v]
  | Seq (e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "c:seq"; to_sexpr e1; to_sexpr e2]
and fundef_to_sexpr x = Sexpr.Sexpr [Sexpr.Sident "c:fundef"; lt_to_sexpr x.fun_name; Sexpr.Sexpr (List.map vt_to_sexpr x.args); Sexpr.Sexpr (List.map vt_to_sexpr x.formal_fv); to_sexpr x.body]

let topvar_to_sexpr tv = Sexpr.Sexpr [lt_to_sexpr tv.var_name; to_sexpr tv.expr]

let topDecl_to_sexpr = function
  | FunDecl fundef -> Sexpr.tagged_sexpr "fun-decl" [fundef_to_sexpr fundef]
  | VarDecl topvar -> Sexpr.tagged_sexpr "var-decl" [topvar_to_sexpr topvar]

let topDecls_to_sexpr tds =
	Sexpr.tagged_sexpr "top-decl-list" (List.map topDecl_to_sexpr tds)
	
let topDecls : topDecl list ref = ref []

