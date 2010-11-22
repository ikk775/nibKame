open TypingExpr
open TypingType


type resultVar = Id.t

type result =
  | R_Constant of Syntax.lit * TypingType.oType
  | R_Variable of resultVar * TypingType.oType
  | R_Fun of (resultVar * TypingType.oType) * result
  | R_Apply of result * result
  | R_Tuple of result list * TypingType.oType
  | R_Vector of result list * TypingType.oType
  | R_If of result * result * result * TypingType.oType
  | R_Let of (resultVar * TypingType.oType) * result * result
  | R_Fix of (resultVar * TypingType.oType) * result * TypingType.oType

let rec substituteResultType ss expr =
  let subst = substituteResultType ss in
  let tsubst = TypingType.substitute ss in
  match expr with
    | R_Variable (v ,t) -> R_Variable (v, tsubst t)
    | R_Constant (v ,t) -> R_Constant (v, tsubst t)
    | R_Fun((v, t), e) -> R_Fun((v, tsubst t), subst e)
    | R_Apply(e1, e2) -> R_Apply(subst e1, subst e2)
    | R_Tuple(es, t) -> R_Tuple(List.map subst es, tsubst t)
    | R_Vector(es, t) -> R_Vector(List.map subst es, tsubst t)
    | R_If(e1, e2, e3, t) -> R_If(subst e1, subst e2, subst e3, tsubst t)
    | R_Let((v, t), e1, e2) -> R_Let((v, tsubst t), subst e1, subst e2)
    | R_Fix((f, t), e, t') -> R_Fix((f, tsubst t), subst e, tsubst t')

let rec w (env:typeEnv) expr =
  match expr with
    | E_Constant c ->
      let t = getConstantType (E_Constant c) in
      [], t, R_Constant(c, t)
    | E_Variable v ->
      let ts = getVariableType env (E_Variable v) in
      let freeTypeVarsTs = freeTypeVars ts in
      let newTypeVars = genTypeVars (List.length freeTypeVarsTs) in
      let subst = List.map (function x, y -> Substitution(x,y)) (List.combine freeTypeVarsTs newTypeVars) in
      let t = substitute subst (removeQuantifier ts) in
      subst, t, R_Variable(v, t)
    | E_Fun(v, expr) -> 
      let b = genTypeVar () in
      let s1, t1, expr' = w (addEnv env v (OType b)) expr in
      let bt = substitute s1 b in
      let t2 = O_Fun(bt, t1) in
      s1, t2, R_Fun((v, bt), expr')
    | E_Apply(e1, e2) -> 
      let b = genTypeVar () in
      let s1, t1, e1' = w env e1 in
      let s2, t2, e2' = w (substituteEnv s1 env) e2 in
      let s3 = unify (substitute s2 t1) (O_Fun(t2, b)) in
      let t = substitute s3 b in
      composite s3 (composite s2 s1), t, R_Apply(e1', e2')
    | E_Tuple(es) -> 
      let rec w_sub env es ss ts e's =
        match es with
          | [] -> ss, ts, e's
          | e :: es' -> 
            let s, t, e' = w env e in
            let env' = (substituteEnv s env) in
            w_sub env' es' (s :: ss) (t :: ts) (e' :: e's)
      in
      let w_results = w_sub env es [] [] [] in
      let ss, ts, e's = w_results in
      let ts' = List.rev ts in
      let e's' = List.rev e's in
(*      let e's'' = List.map2 (fun e' t -> E_Type(e',t)) e's' ts' in *)
      let t = O_Tuple(ts) in
      compositeSubsts ss, t, R_Tuple(e's', t)
    | E_Vector(es) -> 
      let ss, t, e' = w env (E_Tuple es) in
      let ts = (match t with
        | O_Tuple ts -> ts
        | _ -> invalid_arg "unexpected ")
      in
      let e'' = (match e' with
        | R_Tuple (es, t) -> es
        | _ -> invalid_arg "unexpected ")
      in
      let rec u_sub env ss t1 = (function
        | [] -> env, ss, t1
        | t2 :: ts ->
          let t2' = substitute ss t2 in
          let ss' = unify t1 t2' in
          u_sub (substituteEnv ss' env) (composite ss' ss) (substitute ss' t2') ts)
      in
      let b = genTypeVar () in
      let env', ss', t' = u_sub env ss b ts in
      let t'' = O_Vector t' in
      ss', t'', R_Vector(e'', t'')
    | E_If(e1, e2, e3)  -> 
      let s1, t1, e1' = w env e1 in
      let env' = (substituteEnv s1 env) in
      let s2, t2, e2' = w env' e2 in
      let env'' = (substituteEnv s2 env') in
      let s3, t3, e3' = w env'' e3 in
      let s4 = unify t1 (O_Constant Type.Bool) in
      let s5 = unify t2 t3 in
      let t = substitute s5 t2 in
      compositeSubsts [s5;s4;s3;s2;s1], t, R_If(e1', e2', e3',t)
    | E_Let(v, e1, e2)  -> 
      let s1, t1, e1' = w env e1 in
      let s1env = substituteEnv s1 env in
      let s2, t2, e2' = w (addEnv s1env v (clos s1env (OType t1))) e2 in
      composite s2 s1, t2, R_Let((v, t1), e1', e2')
    | E_Fix(f, E_Fun(x, e)) -> 
      let b = genTypeVar () in
      let s1, t1, e' = w (addEnv env f (OType b)) (E_Fun(x, e)) in
      let s2 = unify (substitute s1 b) t1 in
      let t2 = substitute s2 t1 in
      let bt = substitute s2 b in
      composite s2 s1, t2, R_Fix((f, bt), e', t2)
    | _ -> invalid_arg "Invalid expr."

let typing env expr =
  let ss, t, expr' = w env expr in
  t, substituteResultType ss expr'
  