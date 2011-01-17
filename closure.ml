
type closure = { entry: Id.l; actual_fv: Id.t list }
type comp = Eq | NotEq | Ls | LsEq | Gt | GtEq

type t =
  | Unit
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
  | ApplyCls of Id.t * Id.t list
  | ApplyDir of Id.l * Id.t list
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
  | Unit | Int(_) | Float(_) | Char(_) | ExtArray(_) -> Id.Set.empty
  | Neg(x) | FNeg(x) -> Id.Set.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | ArrayRef(x, y) -> Id.Set.of_list [x; y]
  | If(_, x, y, e1, e2) -> Id.Set.add x (Id.Set.add y (Id.Set.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> Id.Set.union (fv e1) (Id.Set.remove x (fv e2))
  | Seq(e1, e2) -> Id.Set.union (fv e1) (fv e2)
  | Var(x) -> Id.Set.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> Id.Set.remove x (Id.Set.union (Id.Set.of_list ys) (fv e))
  | ApplyCls(x, ys) -> Id.Set.of_list (x :: ys)
  | ApplyDir(_, xs) | Tuple(xs) -> Id.Set.of_list xs
  | LetTuple(xts, y, e) -> Id.Set.add y (Id.Set.diff (fv e) (Id.Set.of_list (List.map fst xts)))
  | ArrayAlloc(t, n) -> Id.Set.singleton n
  | ArraySet(x, y, z) -> Id.Set.of_list [x; y; z]
  | Car(x) | Cdr(x) | FCar(x) | FCdr(x) -> Id.Set.singleton x
  | Cons(x, y) | FCons(x, y) -> Id.Set.of_list [x; y]
  | Set(x, y) -> Id.Set.of_list [x; y]
  | Ref(x) -> Id.Set.singleton x

let rec g env known = function (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  | KNormal.Unit -> Unit
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
  | KNormal.IfEq(x, y, e1, e2) -> If(Eq, x, y, g env known e1, g env known e2)
  | KNormal.IfNotEq(x, y, e1, e2) -> If(Eq, x, y, g env known e1, g env known e2)
  | KNormal.IfLs(x, y, e1, e2) -> If(LsEq, x, y, g env known e1, g env known e2)
  | KNormal.IfLsEq(x, y, e1, e2) -> If(LsEq, x, y, g env known e1, g env known e2)
  | KNormal.IfGt(x, y, e1, e2) -> If(LsEq, x, y, g env known e1, g env known e2)
  | KNormal.IfGtEq(x, y, e1, e2) -> If(LsEq, x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (Id.Map.add x t env) known e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.LetFun({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* 関数定義の場合 (caml2html: closure_letrec) *)
      (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
     xに自由変数がない(closureを介さずdirectに呼び出せる)
     と仮定し、knownに追加してe1をクロージャ変換してみる *)
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
  | KNormal.Apply(x, ys) when Id.Set.mem x known -> (* 関数適用の場合 (caml2html: closure_app) *)
      Format.eprintf "directly applying %s@." x;
      ApplyDir(Id.L(x), ys)
  | KNormal.Apply(f, xs) -> ApplyCls(f, xs)
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
  | KNormal.ExtFunApply((x, t), ys) -> ApplyDir(Id.L("_nibkame_" ^ x ^ "_T" ^ Type.to_string t), ys)

let f e =
  topDecls := [];
  let e' = g Id.Map.empty Id.Set.empty e in
  List.rev !topDecls, e'

let from_knormal k = fst (f k)
