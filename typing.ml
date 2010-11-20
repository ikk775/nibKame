open TypingExpr
open TypingType

let rec w (env:typeEnv) expr =
  match expr with
    | E_Constant c ->
      let t = getConstantType (E_Constant c) in
      [], t, E_Type(expr, t)
    | E_Variable v ->
      let ts = getVariableType env (E_Variable v) in
      let freeTypeVarsTs = freeTypeVars ts in
      let newTypeVars = genTypeVars (List.length freeTypeVarsTs) in
      let subst = List.map (function x, y -> Substitution(x,y)) (List.combine freeTypeVarsTs newTypeVars) in
      let t = substitute subst (removeQuantifier ts) in
      subst, t, E_Type(expr, t)
    | E_Fun(v, expr) -> 
      let b = genTypeVar () in
      let s1, t1, expr' = w (addEnv env v (OType b)) expr in
      let bt = substitute s1 b in
      let t2 = O_Fun(bt, t1) in
      s1, t2, E_Type(E_Fun(v, E_Declare(v, bt, expr')), t2)
    | E_Apply(e1, e2) -> 
      let b = genTypeVar () in
      let s1, t1, e1' = w env e1 in
      let s2, t2, e2' = w (substituteEnv s1 env) e2 in
      let s3 = unify (substitute s2 t1) (O_Fun(t2, b)) in
      let t = substitute s3 b in
      composite s3 (composite s2 s1), t, E_Type(E_Apply(e1', e2'), t)
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
      let e's'' = List.map2 (fun e' t -> E_Type(e',t)) e's' ts' in
      let t = O_Tuple(ts) in
      compositeSubsts ss, t, E_Type(E_Tuple(e's''), t)
    | E_Vector(es) -> 
      let ss, t, e' = w env (E_Tuple es) in
      let ts = (match t with
        | O_Tuple ts -> ts
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
      ss', O_Vector t', e'
    | E_If(e1, e2, e3)  -> 
      let s1, t1, e1' = w env e1 in
      let env' = (substituteEnv s1 env) in
      let s2, t2, e2' = w env' e2 in
      let env'' = (substituteEnv s2 env') in
      let s3, t3, e3' = w env'' e3 in
      let s4 = unify t1 (O_Constant Type.Bool) in
      let s5 = unify t2 t3 in
      let t = substitute s5 t2 in
      compositeSubsts [s5;s4;s3;s2;s1], t, E_Type(E_If(E_Type(e1',t1),E_Type(e2',t2),E_Type(e3',t3)),t)
    | E_Let(v, e1, e2)  -> 
      let s1, t1, e1' = w env e1 in
      let s1env = substituteEnv s1 env in
      let s2, t2, e2' = w (addEnv s1env v (clos s1env (OType t1))) e2 in
      composite s2 s1, t2, E_Type(E_Let(v, e1', E_Declare(v, t1, e2')), t2)
    | E_Fix(f, E_Fun(x, e)) -> 
      let b = genTypeVar () in
      let s1, t1, e' = w (addEnv env f (OType b)) (E_Fun(x, e)) in
      let s2 = unify (substitute s1 b) t1 in
      let t2 = substitute s2 t1 in
      let bt = substitute s2 b in
      composite s2 s1, t2, E_Type(E_Fix(f, E_Declare(f, bt, E_Type(E_Fun(x, e'), t1))), t2)
    | _ -> invalid_arg "Invalid expr."

let typing env expr =
  let ss, t, expr' = w env expr in
  t, substituteExprType ss expr'
  