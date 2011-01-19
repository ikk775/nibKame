(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal

let find x env = try Id.Map.find x env with Not_found -> x

let rec g env = function (* α変換ルーチン本体 (caml2html: alpha_g) *)
  | Unit -> Unit
  | Nil(tc) -> Nil(tc)
  | Char(i) -> Char(i)
  | Int(i) -> Int(i)
  | Float(d) -> Float(d)
  | Neg(x) -> Neg(find x env)
  | Add(x, y) -> Add(find x env, find y env)
  | Sub(x, y) -> Sub(find x env, find y env)
  | Mul(x, y) -> Mul(find x env, find y env)
  | Div(x, y) -> Div(find x env, find y env)
  | FNeg(x) -> FNeg(find x env)
  | FAdd(x, y) -> FAdd(find x env, find y env)
  | FSub(x, y) -> FSub(find x env, find y env)
  | FMul(x, y) -> FMul(find x env, find y env)
  | FDiv(x, y) -> FDiv(find x env, find y env)
  | IfEq(x, y, e1, e2) -> IfEq(find x env, find y env, g env e1, g env e2)
  | IfNotEq(x, y, e1, e2) -> IfEq(find x env, find y env, g env e1, g env e2)
  | IfGt(x, y, e1, e2) -> IfLsEq(find x env, find y env, g env e1, g env e2)
  | IfGtEq(x, y, e1, e2) -> IfLsEq(find x env, find y env, g env e1, g env e2)
  | IfLs(x, y, e1, e2) -> IfLsEq(find x env, find y env, g env e1, g env e2)
  | IfLsEq(x, y, e1, e2) -> IfLsEq(find x env, find y env, g env e1, g env e2)
  | Let((x, t), e1, e2) -> (* letのα変換 (caml2html: alpha_let) *)
      let x' = KNormal.gen_varname () in
      Let((x', t), g env e1, g (Id.Map.add x x' env) e2)
  | Var(x) -> Var(find x env)
  | LetFun({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recのα変換 (caml2html: alpha_letrec) *)
      let env = Id.Map.add x (KNormal.gen_varname ()) env in
      let ys = List.map fst yts in
      let env' = Id.Map.add_list2 ys (KNormal.gen_varnames (List.length ys)) env in
      LetFun({ name = (find x env, t);
	       args = List.map (fun (y, t) -> (find y env', t)) yts;
	       body = g env' e1 },
	     g env e2)
  | Apply((x, t), ys) -> Apply((find x env, t), List.map (fun y -> find y env) ys)
  | Tuple(xs) -> Tuple(List.map (fun x -> find x env) xs)
  | LetTuple(xts, y, e) -> (* LetTupleのα変換 (caml2html: alpha_lettuple) *)
      let xs = List.map fst xts in
      let env' = Id.Map.add_list2 xs (KNormal.gen_varnames (List.length xs)) env in
      LetTuple(List.map (fun (x, t) -> (find x env', t)) xts,
	       find y env,
	       g env' e)
  | ArrayRef(x, y) -> ArrayRef(find x env, find y env)
  | ArraySet(x, y, z) -> ArraySet(find x env, find y env, find z env)
  | ExtArray(x) -> ExtArray(x)
  | ExtFunApply(x, ys) -> ExtFunApply(x, List.map (fun y -> find y env) ys)
  | Ref(x) -> Ref(find x env)
  | Set(x, y) -> Set(find x env, find y env)
  | Cdr(x) -> Cdr(find x env)
  | Car(x) -> Car(find x env)
  | Cons(x, y) -> Cons(find x env, find y env)
  | FCdr(x) -> FCdr(find x env)
  | FCar(x) -> FCar(find x env)
  | FCons(x, y) -> FCons(find x env, find y env)
  | ArrayAlloc(t, x) -> ArrayAlloc(t, find x env)

let f = g Id.Map.empty
