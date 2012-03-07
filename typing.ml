open MyUtil
open TypingExpr
open TypingType

module E = TypingExpr
module T = TypingType


type resultVar = Id.t

type result =
  | R_Constant of Syntax.lit * TypingType.oType
  | R_Variable of resultVar * TypingType.oType
  | R_Fun of (resultVar * TypingType.oType) * result
  | R_Apply of result * result
  | R_Tuple of result list * TypingType.oType
  | R_Vector of result list * TypingType.oType
  | R_If of result * result * result
  | R_Let of (resultVar * TypingType.oType) * result * result
  | R_Fix of (resultVar * TypingType.oType) * result * TypingType.oType
  | R_Match of result * (pattern * result option * result) list
  | R_External of Id.t * TypingType.oType
and pattern =
  | RP_Constant of Syntax.lit * TypingType.oType
  | RP_Variable of Id.t option * TypingType.oType
  | RP_Constructor of Id.t * TypingType.oType
  | RP_Apply of (pattern * pattern) * TypingType.oType
  | RP_And of (pattern * pattern) * TypingType.oType
  | RP_Or of (pattern * pattern) * TypingType.oType (* Both patterns must have a same set of variables. And each variable has same type across the patterns. *)
  | RP_Not of pattern * TypingType.oType
  | RP_Tuple of pattern list * TypingType.oType
  | RP_Vector of pattern list * TypingType.oType
and clause = pattern * result option * result

let rec map_pattern_leaf f_leaf pat =
  let rec g pat = match pat with
    | RP_Constant _ | RP_Variable _ | RP_Constructor _ -> f_leaf pat
    | RP_Apply ((p1, p2), t) -> RP_Apply ((g p1, g p2), t)
    | RP_And ((p1, p2), t) -> RP_And ((g p1, g p2), t)
    | RP_Or ((p1, p2), t) -> RP_Or ((g p1, g p2), t)
    | RP_Not (p, t) -> RP_Not ((g p), t)
    | RP_Tuple (ps, t) -> RP_Tuple ((List.map g ps), t)
    | RP_Vector (ps, t) -> RP_Vector ((List.map g ps), t)
  in
  g pat

let rec map_pattern_type f pat =
  let rec g pat = match pat with
    | RP_Constant (c, t) -> RP_Constant (c, f t)
    | RP_Variable (v, t) -> RP_Variable (v, f t)
    | RP_Constructor (v, t) -> RP_Constructor (v, f t)
    | RP_Apply ((p1, p2), t) -> RP_Apply ((g p1, g p2), f t)
    | RP_And ((p1, p2), t) -> RP_And ((g p1, g p2), f t)
    | RP_Or ((p1, p2), t) -> RP_Or ((g p1, g p2), f t)
    | RP_Not (p, t) -> RP_Not ((g p), f t)
    | RP_Tuple (ps, t) -> RP_Tuple ((List.map g ps), f t)
    | RP_Vector (ps, t) -> RP_Vector ((List.map g ps), f t)
  in
  g pat

let rec walk_pattern_leaf f_leaf pat =
  let rec g pat = match pat with
    | RP_Constant _ | RP_Variable _ | RP_Constructor _ -> f_leaf pat
    | RP_Apply ((p1, p2), t) | RP_And ((p1, p2), t) | RP_Or ((p1, p2), t) -> g p1 @ g p2
    | RP_Not (p, t) -> g p
    | RP_Tuple (ps, t) | RP_Vector (ps, t) -> List.concat (List.map g ps)
  in
  g pat

let rec walk_pattern_leaf_type f pat =
  let rec g pat = match pat with
    | RP_Constant (_, t) | RP_Variable (_, t) | RP_Constructor (_, t) -> f t
    | RP_Apply ((p1, p2), t) | RP_And ((p1, p2), t) | RP_Or ((p1, p2), t) -> g p1 @ g p2
    | RP_Not (p, t) -> g p
    | RP_Tuple (ps, t) | RP_Vector (ps, t) -> List.concat (List.map g ps)
  in
  g pat

let rec patternvars = function
  | RP_Constant _ -> []
  | RP_Constructor _ -> []
  | RP_Variable (None, t) -> []
  | RP_Variable (Some v, t) -> [v, t]
  | RP_Apply ((p1, p2), t) | RP_And ((p1, p2), t) | RP_Or ((p1, p2), t) -> List.unique (patternvars p1 @ patternvars p2)
  | RP_Not (p, t) -> patternvars p
  | RP_Tuple (ps, t) | RP_Vector (ps, t) -> List.unique (List.concat (List.map patternvars ps))

let rec boundvars : result -> (Id.t * TypingType.oType) list = function
  | R_Constant (l, t) -> []
  | R_Variable (v, t) -> []
  | R_Fun((v, t), e) -> (v, t) :: boundvars e
  | R_Apply(e1, e2) -> List.append (boundvars e1) (boundvars e2)
  | R_Tuple (es, t) -> List.concat (List.map boundvars es)
  | R_Vector (es, t) -> List.concat (List.map boundvars es)
  | R_If (e1, e2, e3) -> List.concat (List.map boundvars [e1; e2; e3])
  | R_Let ((v, t), e1, e2) -> (v, t) :: List.concat (List.map boundvars [e1; e2])
  | R_Fix ((v, t), e, t') -> (v, t) :: boundvars e
  | R_External _ -> []
  | R_Match (e, cls) -> 
    let g = function
      | pat, None, expr -> 
        patternvars pat @ boundvars expr
      | pat, Some guard, expr -> 
        patternvars pat @ boundvars guard @ boundvars expr
    in
    boundvars e @ List.concat (List.map g cls)
    

let rec pattern_freevars = function
  | RP_Variable (None, t) -> []
  | RP_Variable (Some v, t) -> [v, t]
  | RP_Constant _ | RP_Constructor _ -> []
  | RP_Apply ((p1, p2), t) | RP_And ((p1, p2), t) | RP_Or ((p1, p2), t) -> List.unique (List.concat (List.map pattern_freevars [p1; p2]))
  | RP_Not (p, t) -> pattern_freevars p
  | RP_Tuple (ps, t) | RP_Vector (ps, t) -> List.unique (List.concat (List.map pattern_freevars ps))

let rec freevars : result -> (Id.t * TypingType.oType) list = function
  | R_Constant (l, t) -> []
  | R_Variable (v, t) -> [(v, t)]
  | R_Fun((v, t), e) -> List.remove_assoc v (freevars e)
  | R_Apply(e1, e2) -> List.unique (List.append (freevars e1) (freevars e2))
  | R_Tuple (es, t) -> List.unique (List.concat (List.map freevars es))
  | R_Vector (es, t) -> List.unique (List.concat (List.map freevars es))
  | R_If (e1, e2, e3) -> List.unique (List.concat (List.map freevars [e1; e2; e3]))
  | R_Let ((v, t), e1, e2) -> List.remove_assoc v(List.unique (List.concat (List.map freevars [e1; e2]))) 
  | R_Fix ((v, t), e, t') -> List.remove_assoc v (freevars e)
  | R_External _ -> []
  | R_Match (e, cls) -> 
    let f = function
      | pat, None, expr -> 
        List.setDiff (List.unique (freevars expr)) (pattern_freevars pat)
      | pat, Some guard, expr -> 
        List.setDiff (List.unique (freevars guard @ freevars expr)) (pattern_freevars pat)
    in
    freevars e @ List.concat (List.map f cls)

type substitution = ((resultVar * TypingType.oType) * result) (* 置換元と置換先の型は一致している必要がある *)

let rec typevars : result -> Id.t list = fun r -> 
  let ftv t = TypingType.freetypevars (TypingType.OType t) in
  let rec g = function
    | R_Constant (l, t) -> [ftv t]
    | R_Variable (v, t) -> [ftv t]
    | R_Fun((v, t), e) -> List.unique (ftv t :: g e)
    | R_Apply(e1, e2) -> List.unique (List.append (g e1) (g e2))
    | R_Tuple (es, t) -> List.unique (List.concat (List.map g es))
    | R_Vector (es, t) -> List.unique (List.concat (List.map g es))
    | R_If (e1, e2, e3) -> List.unique (List.concat (List.map g [e1; e2; e3]))
    | R_Let ((v, t), e1, e2) -> List.unique (ftv t :: (List.concat (List.map g [e1; e2]))) 
    | R_Fix ((v, t), e, t') -> List.unique (ftv t :: g e)
    | R_External (v, t) -> [ftv t]
    | R_Match (e, cls) -> 
      let f = function
        | pat, None, expr -> 
          List.unique (walk_pattern_leaf_type ftv pat :: g expr)
        | pat, Some guard, expr -> 
          List.unique (walk_pattern_leaf_type ftv pat :: g guard @ g expr)
      in
      List.unique (g e @ List.concat (List.map f cls))
  in
  List.concat (g r)

let is_variable = function
  | R_Constant _ -> true

let gen_var_num = ref 0

let gen_varname () =
  gen_var_num := !gen_var_num + 1;
  Format.sprintf "$r:%d" !gen_var_num

let gen_var t =
  let b = gen_varname () in
  R_Variable (b, t)

let rec gen_varnames n =
  List.iter_list n gen_varname

let varname = function
  | R_Variable (v, t) -> v
  | _ -> invalid_arg "R_Variable expected."

let rec result_freetypevars : (Id.t * TypingType.oType) list -> Id.t list = fun vts -> 
  List.concat (List.map (function x, t -> TypingType.freetypevars (TypingType.OType t)) vts)

let rec safe_typevars r =
  let rec f = function
    | R_Fun ((v, t), r) -> t :: f r
    | R_Fix ((v, t), r, t') -> f r
    | _ -> [] in
  let ots = f r in
  MyUtil.List.unique (List.concat (List.map (fun ot -> TypingType.freetypevars (TypingType.OType ot)) ots))
  
let rec value_restrict : result -> TypingType.oType -> TypingType.typeScheme = fun r t -> 
  let ftv = safe_typevars r in
  TypingType.QType (ftv, TypingType.OType t)

let rec substitute_result_type ss expr =
  let subst = substitute_result_type ss in
  let tsubst = TypingType.substitute ss in
  match expr with
    | R_Variable (v ,t) -> R_Variable (v, tsubst t)
    | R_Constant (v ,t) -> R_Constant (v, tsubst t)
    | R_Fun((v, t), e) -> R_Fun((v, tsubst t), subst e)
    | R_Apply(e1, e2) -> R_Apply(subst e1, subst e2)
    | R_Tuple(es, t) -> R_Tuple(List.map subst es, tsubst t)
    | R_Vector(es, t) -> R_Vector(List.map subst es, tsubst t)
    | R_If(e1, e2, e3) -> R_If(subst e1, subst e2, subst e3)
    | R_Let((v, t), e1, e2) -> R_Let((v, tsubst t), subst e1, subst e2)
    | R_Fix((f, t), e, t') -> R_Fix((f, tsubst t), subst e, tsubst t')
    | R_External (v ,t) -> R_External (v, tsubst t)
    | R_Match (e, cls) -> 
      let g = function
        | pat, None, expr -> 
          map_pattern_type tsubst pat, None, subst expr
        | pat, Some guard, expr -> 
          map_pattern_type tsubst pat, Some (subst guard), subst expr
      in
      R_Match(subst e, List.map g cls)
and substitute_pattern_type ss pat =
  let tsubst = TypingType.substitute ss in
  map_pattern_type tsubst pat

let rec result_to_expr expr =
  let to_expr = result_to_expr in
  match expr with
    | R_Variable (v ,t) -> E_Variable (v)
    | R_Constant (v ,t) -> E_Constant (v)
    | R_Fun((v, t), e) -> E_Fun(v, to_expr e)
    | R_Apply(e1, e2) -> E_Apply(to_expr e1, to_expr e2)
    | R_Tuple(es, t) -> E_Tuple(List.map to_expr es)
    | R_Vector(es, t) -> E_Vector(List.map to_expr es)
    | R_If(e1, e2, e3) -> E_If(to_expr e1, to_expr e2, to_expr e3)
    | R_Let((v, t), e1, e2) -> E_Let(v, to_expr e1, to_expr e2)
    | R_Fix((f, t), e, t') -> E_Fix(f, to_expr e)
    | R_External (v, t) -> E_External (v, t)
    | R_Match (e, cls) -> 
      let rec g = function
        | RP_Variable (None, _) -> EP_Variable None
        | RP_Variable (Some v, _) -> EP_Variable (Some v)
        | RP_Constant (lit, _) -> EP_Constant lit
        | RP_Constructor (v, _) -> EP_Constructor v
        | RP_Apply ((p1, p2), _) -> EP_Apply (g p1, g p2)
        | RP_And ((p1, p2), _) -> EP_And (g p1, g p2)
        | RP_Or ((p1, p2), _) -> EP_Or (g p1, g p2)
        | RP_Not (p, _) -> EP_Not (g p)
        | RP_Tuple (ps, _) -> EP_Tuple (List.map g ps)
        | RP_Vector (ps, _) -> EP_Vector (List.map g ps)
      in
      let f = function
        | pat, None, expr -> 
          g pat, None, to_expr expr
        | pat, Some guard, expr -> 
          g pat, Some (to_expr guard), to_expr expr
      in
      E_Match (to_expr e, List.map f cls)

let rec w env expr =
  match expr with
    | E_Constant c ->
      let t = E.get_constant_type (E_Constant c) in
      [], t, R_Constant(c, t)
    | E_External (v, t) -> 
      [], t, R_External (v, t)
    | E_Variable v ->
      let ts = E.get_variable_type env (E_Variable v) in
      let boundTypeVarsTs = TypingType.boundVars ts in
      let newTypeVars = T.gen_typevars (List.length boundTypeVarsTs) in
      let subst = List.map (function x, y -> Substitution(x,y)) (List.combine boundTypeVarsTs newTypeVars) in
      let t = substitute subst (T.remove_quantifier ts) in
      subst, t, R_Variable(v, t)
    | E_Fun(v, expr) -> 
      let b = T.gen_typevar () in
      let s1, t1, expr' = w (TypingExpr.add_env env v (OType b)) expr in
      let bt = substitute s1 b in
      let t2 = O_Fun(bt, t1) in
      s1, t2, R_Fun((v, bt), expr')
    | E_Apply(e1, e2) -> 
      let b = T.gen_typevar () in
      let s1, t1, e1' = w env e1 in
      let s2, t2, e2' = w (TypingExpr.substitute_env s1 env) e2 in
      let s3 = unify (substitute s2 t1) (O_Fun(t2, b)) in
      let t = substitute s3 b in
      compose s3 (compose s2 s1), t, R_Apply(e1', e2')
    | E_Tuple(es) -> 
      let rec w_sub env es ss ts e's =
        match es with
          | [] -> ss, ts, e's
          | e :: es' -> 
            let s, t, e' = w env e in
            let env' = (TypingExpr.substitute_env s env) in
            w_sub env' es' (s :: ss) (t :: ts) (e' :: e's)
      in
      let w_results = w_sub env es [] [] [] in
      let ss, ts, e's = w_results in
      let e's' = List.rev e's in
      let t = O_Tuple(ts) in
      T.compose_substs ss, t, R_Tuple(e's', t)
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
          u_sub (TypingExpr.substitute_env ss' env) (compose ss' ss) (substitute ss' t2') ts)
      in
      let b = T.gen_typevar () in
      let env', ss', t' = u_sub env ss b ts in
      let t'' = O_Vector t' in
      ss', t'', R_Vector(e'', t'')
    | E_If(e1, e2, e3)  -> 
      let s1, t1, e1' = w env e1 in
      let env' = (TypingExpr.substitute_env s1 env) in
      let s2, t2, e2' = w env' e2 in
      let env'' = (TypingExpr.substitute_env s2 env') in
      let s3, t3, e3' = w env'' e3 in
      let s4 = unify t1 (O_Constant Type.Bool) in
      let s5 = unify t2 t3 in
      let t = substitute s5 t2 in
      T.compose_substs [s5;s4;s3;s2;s1], t, R_If(e1', e2', e3')
    | E_Let(v, e1, e2)  -> 
      let s1, t1, e1' = w env e1 in
      let s1env = TypingExpr.substitute_env s1 env in
      let s2, t2, e2' = w (TypingExpr.add_env s1env v (TypingExpr.clos s1env (OType t1))) e2 in
      compose s2 s1, t2, R_Let((v, t1), e1', e2')
    | E_Fix(f, E_Fun(x, e)) -> 
      let b = T.gen_typevar () in
      let s1, t1, e' = w (TypingExpr.add_env env f (OType b)) (E_Fun(x, e)) in
      let s2 = unify (substitute s1 b) t1 in
      let t2 = substitute s2 t1 in
      let bt = substitute s2 b in
      compose s2 s1, t2, R_Fix((f, bt), e', t2)
    | E_Fix(f, _) -> failwith "A fix operator must be followed by a fun operator."
    | E_Declare (v, t, e) ->
      w (E.add_env env v (T.OType t)) e
    | E_Type (e, t) ->
      let s, t', e' = w env e in
      let s' = unify (substitute s t') t in
      let t'' = substitute s' t' in
      compose s' s, t'', e'
    | E_Match (e, cls) ->
      let f = function (ss, env, tpat, texpr), cls ->
        let g = function
          | pat, None, expr -> 
            let sps, tp, p' = w_pattern env pat in
            let sps' = unify tp tpat in
            let tpat' = T.substitute sps' tp in
            let p'' = substitute_pattern_type (T.compose sps' sps) p' in
            let vts = patternvars p'' in
            let vs = List.map fst vts in
            if List.length vs <> List.length (List.unique vs)
            then failwith "Variable names must be unique in the pattern."
            else ();
            let env' = E.combine_env env (E.ExprEnv (List.map (function v, t -> v, T.OType t) vts)) in
            Debug.dbgprint "env':";
            Debug.dbgprintsexpr (E.exprEnv_to_sexpr env');
            let ses, te, e' = w env' expr in
            let ses' = unify te texpr in
            let texpr' = T.substitute ses' te in
            let e'' = substitute_result_type ses' e' in
            let ss = T.compose_substs [ses'; ses; sps'; sps; ss] in
            (ss, E.substitute_env ss env, tpat', texpr'), (p'', None, e'') :: cls
          | pat, Some guard, expr -> 
            let sps, tp, p' = w_pattern env pat in
            let sps' = unify tp tpat in
            let tpat' = T.substitute sps' tp in
            let p'' = substitute_pattern_type (T.compose sps' sps) p' in
            let vts = patternvars p'' in
            let vs = List.map fst vts in
            if List.length vs <> List.length (List.unique vs)
            then failwith "Variable names must be unique in the pattern."
            else ();
            let env' = E.combine_env env (E.ExprEnv (List.map (function v, t -> v, T.OType t) vts)) in
            Debug.dbgprint "env':";
            Debug.dbgprintsexpr (E.exprEnv_to_sexpr env');
            let sgs, tg, g' = w env' guard in
            let sgs' = unify tg (O_Constant Type.Bool) in
            let g'' = substitute_result_type sgs' g' in
            let env'' = E.substitute_env (T.compose sgs' sgs) env' in
            let ses, te, e' = w env'' expr in
            let ses' = unify te texpr in
            let texpr' = T.substitute ses' te in
            let e'' = substitute_result_type ses' e' in
            let ss = T.compose_substs [ses'; ses; sgs'; sgs; sps'; sps; ss] in
            (ss, E.substitute_env ss env, tpat', texpr'), (p'', Some g'', e'') :: cls
        in
        g
      in
      let ses, te, e' = w env e in
      let env' = E.substitute_env ses env in
      let b1 = T.gen_typevar () in
      let b2 = T.gen_typevar () in
      let rstcls = List.fold_left f ((ses, env', b1, b2), []) cls in
      let ss, env, tclp, tcle = fst rstcls in
      let rcls = snd rstcls in
      let tclp' = T.substitute ss tclp in
      let tcle' = T.substitute ss tcle in
      let ses' = unify te tclp' in
      T.compose_substs [ses'; ses; ss], tcle', R_Match (e', rcls)
and w_pattern env = function
  | EP_Constant c ->
    let t = E.get_constant_type (E_Constant c) in
    [], t, RP_Constant(c, t)
  | EP_Variable None ->
    let t = T.gen_typevar () in
    [], t, RP_Variable (None, t)
  | EP_Variable (Some v) ->
    let t = T.gen_typevar () in
    [], t, RP_Variable (Some v, t)
  | EP_Constructor v -> 
    let ts = E.get_variable_type env (E_Variable v) in
    let boundTypeVarsTs = TypingType.boundVars ts in
    let newTypeVars = T.gen_typevars (List.length boundTypeVarsTs) in
    let subst = List.map (function x, y -> Substitution(x,y)) (List.combine boundTypeVarsTs newTypeVars) in
    let t = substitute subst (T.remove_quantifier ts) in
    subst, t, RP_Constructor (v, t)
  | EP_Apply(p1, p2) -> 
    let b = T.gen_typevar () in
    let s1, t1, p1' = w_pattern env p1 in
    let s2, t2, p2' = w_pattern (TypingExpr.substitute_env s1 env) p2 in
    let s3 = unify (substitute s2 t1) (O_Fun(t2, b)) in
    let t = substitute s3 b in
    compose s3 (compose s2 s1), t, RP_Apply((p1', p2'), t)
  | EP_Tuple(ps) -> 
    let rec w_pattern_sub env ps ss ts p's =
      match ps with
        | [] -> ss, ts, p's
        | p :: ps' -> 
          let s, t, p' = w_pattern env p in
          let env' = (TypingExpr.substitute_env s env) in
          w_pattern_sub env' ps' (s :: ss) (t :: ts) (p' :: p's)
    in
    let w_results = w_pattern_sub env ps [] [] [] in
    let ss, ts, p's = w_results in
    let p's' = List.rev p's in
    let t = O_Tuple(ts) in
    T.compose_substs ss, t, RP_Tuple(p's', t)
  | EP_Vector(ps) -> 
    let ss, t, p' = w_pattern env (EP_Tuple ps) in
    let ts = (match t with
      | O_Tuple ts -> ts
      | _ -> invalid_arg "unexpected ")
    in
    let p'' = (match p' with
      | RP_Tuple (ps, t) -> ps
      | _ -> invalid_arg "unexpected ")
    in
    let rec u_sub env ss t1 = (function
      | [] -> env, ss, t1
      | t2 :: ts ->
        let t2' = substitute ss t2 in
        let ss' = unify t1 t2' in
        u_sub (TypingExpr.substitute_env ss' env) (compose ss' ss) (substitute ss' t2') ts)
    in
    let b = T.gen_typevar () in
    let env', ss', t' = u_sub env ss b ts in
    let t'' = O_Vector t' in
    ss', t'', RP_Vector(p'', t'')
  | EP_And(p1, p2) -> 
    let ss, t, p' = w_pattern env (EP_Vector [p1; p2]) in
    let t' = (match t with
      | O_Vector ts -> ts
      | _ -> invalid_arg "unexpected ")
    in
    let p1', p2' = (match p' with
      | RP_Vector ([p1; p2], t) -> p1, p2
      | _ -> invalid_arg "unexpected ")
    in
    ss, t', RP_And ((p1', p2'), t')
  | EP_Or(p1, p2) -> 
    let ss, t, p' = w_pattern env (EP_And (p1, p2)) in
    let p1', p2' = (match p' with
      | RP_And ((p1, p2), t) -> p1, p2
      | _ -> invalid_arg "unexpected ")
    in
    ss, t, RP_Or ((p1', p2'), t)
  | EP_Not p -> 
    let ss, t, p' = w_pattern env p in
    ss, t, RP_Not (p', t)

let typing_without_value_restriction env expr =
  let ss, t, expr' = w env expr in
  let expr'' = substitute_result_type ss expr' in
  t, expr'', ss

let typing_with_subst env expr =
  let ss, t, expr' = w env expr in
  let expr'' = substitute_result_type ss expr' in
  value_restrict expr'' t, expr'', ss

let typing env expr =
  let ss, t, expr' = w env expr in
  let expr'' = substitute_result_type ss expr' in
  value_restrict expr'' t, expr''
  
let rec substitute : substitution list -> result -> result = fun ss expr -> 
  let subst = substitute ss in
  let subst' v = substitute (List.filter (function (v', t'), c -> v' <> v) ss) in
  let subst'' vs = substitute (List.filter (function (v', t'), c -> not (List.mem v' vs)) ss) in
  match expr with
    | R_Variable (v ,t) ->
      if List.exists (function (v', t'), c -> v = v' && t = t') ss
      then snd (List.find (function (v', t'), c -> v = v' && t = t') ss)
      else R_Variable (v, t)
    | R_Constant (v ,t) as e -> e
    | R_Fun((v, t), e) -> R_Fun((v, t), subst' v e)
    | R_Apply(e1, e2) -> R_Apply(subst e1, subst e2)
    | R_Tuple(es, t) -> R_Tuple(List.map subst es, t)
    | R_Vector(es, t) -> R_Vector(List.map subst es, t)
    | R_If(e1, e2, e3) -> R_If(subst e1, subst e2, subst e3)
    | R_Let((v, t), e1, e2) -> R_Let((v, t), subst e1, subst' v e2)
    | R_Fix((f, t), e, t') -> R_Fix((f, t), subst' f e, t')
    | R_External (v ,t) as e -> e
    | R_Match (e, cls) ->
      let g = function
        | pat, None, expr -> 
          let pvs = List.map fst (patternvars pat) in
          pat, None, subst'' pvs expr
        | pat, Some guard, expr -> 
          let pvs = List.map fst (patternvars pat) in
          pat, Some (subst'' pvs guard), subst'' pvs expr
      in
      R_Match(subst e, List.map g cls)

let rec substitute_with_expr_subst = fun ss expr -> 
  let subst = substitute_with_expr_subst ss in
  let subst'' vs = substitute_with_expr_subst (List.filter (function v', c -> not (List.mem v' vs)) ss) in
  let subst' v = substitute_with_expr_subst (List.filter (function v', c -> v' <> v) ss) in
  let rec f ss = function
    | R_Variable (v ,t) ->
      let e = TypingExpr.substitute_expr ss (TypingExpr.E_Variable v) in
      begin match e with
        | E_Variable v' -> R_Variable (v', t)
        | _ -> failwith "Substituting a variable by a expression without one variable is not supported yet."
      end
    | R_Constant (v ,t) as e -> e
    | R_Fun((v, t), e) -> R_Fun((v, t), subst' v e)
    | R_Apply(e1, e2) -> R_Apply(subst e1, subst e2)
    | R_Tuple(es, t) -> R_Tuple(List.map subst es, t)
    | R_Vector(es, t) -> R_Vector(List.map subst es, t)
    | R_If(e1, e2, e3) -> R_If(subst e1, subst e2, subst e3)
    | R_Let((v, t), e1, e2) -> R_Let((v, t), subst e1, subst' v e2)
    | R_Fix((f, t), e, t') -> R_Fix((f, t), subst' f e, t')
    | R_External (v ,t) as e -> e
    | R_Match (e, cls) ->
      let g = function
        | pat, None, expr -> 
          let pvs = List.map fst (patternvars pat) in
          pat, None, subst'' pvs expr
        | pat, Some guard, expr -> 
          let pvs = List.map fst (patternvars pat) in
          pat, Some (subst'' pvs guard), subst'' pvs expr
      in
      R_Match(subst e, List.map g cls)
  in
  f ss expr

let rec substitute_varname ss expr =
  let ss' = List.map (function vf, vt -> vf, E_Variable vt) ss in
  substitute_with_expr_subst ss' expr

let rec of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "r:constant"; l; t] -> R_Constant (Syntax.lit_of_sexpr l, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "r:var"; Sexpr.Sident v; t] -> R_Variable (v, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "r:fun"; Sexpr.Sexpr vs; e] -> 
    let fun_nest e' vs =
      let rec fun_nest_sub = function
        | [v] -> 
          (match of_sexpr v with
            | R_Variable(v, t) -> R_Fun((v, t), of_sexpr e')
            | _ -> invalid_arg "unexpected token.")
        | v :: vs ->
          (match of_sexpr v with
            | R_Variable(v, t) -> R_Fun((v, t), fun_nest_sub vs)
            | _ -> invalid_arg "unexpected token.")
        | _ -> invalid_arg "unexpected token."
      in
      fun_nest_sub vs
    in
    fun_nest e vs
  | Sexpr.Sexpr (Sexpr.Sident "r:apply" :: e1 :: e2 :: es) -> 
    let rec apply_nest = function
      | e1 :: e2 :: []->  R_Apply(of_sexpr e1, of_sexpr e2)
      | e :: es -> R_Apply(of_sexpr e, apply_nest es)
      | _ -> invalid_arg "unexpected token."
    in
    apply_nest (e1 :: e2 :: es)
  | Sexpr.Sexpr (Sexpr.Sident "r:tuple" :: t :: es) -> R_Tuple(List.map of_sexpr es, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr (Sexpr.Sident "r:vector" :: t :: es) -> R_Vector(List.map of_sexpr es, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "r:if" ; e1 ; e2; e3] -> R_If(of_sexpr e1, of_sexpr e2, of_sexpr e3)
  | Sexpr.Sexpr [Sexpr.Sident "r:let"; v; e1; e2] -> 
    (match of_sexpr v with
      | R_Variable(v, t) -> R_Let((v, t), of_sexpr e1, of_sexpr e2)
      | _ -> invalid_arg "unexpected token.")
  | Sexpr.Sexpr [Sexpr.Sident "r:fix"; v; e; t'] -> 
    (match of_sexpr v with
      | R_Variable(v, t) -> R_Fix((v, t), of_sexpr e, TypingType.oType_of_sexpr t')
      | _ -> invalid_arg "unexpected token.")
  | Sexpr.Sexpr [Sexpr.Sident "r:external-symbol"; Sexpr.Sident v; t] -> R_External (v, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr (Sexpr.Sident "r:match" :: e :: cls) -> 
    let f = function
      | Sexpr.Sexpr [pat; e] ->
        pattern_of_sexpr pat, None, of_sexpr e
      | Sexpr.Sexpr [pat; Sexpr.Sident ":"; guard; e] ->
        pattern_of_sexpr pat, Some (of_sexpr guard), of_sexpr e
      | _ -> invalid_arg "unexpected token."
    in
    R_Match (of_sexpr e, List.map f cls)
  | _ -> invalid_arg "unexpected token."
and pattern_of_sexpr =
  let nest f initial list =
    let rec g p = function
      | [] -> p
      | p' :: ps' -> g (f p p') ps'
    in
    g initial list
  in function
  | Sexpr.Sexpr [Sexpr.Sident "rp:constant"; lit; t] -> RP_Constant (Syntax.lit_of_sexpr lit, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "rp:any"; t] -> RP_Variable (None, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "rp:var"; Sexpr.Sident v; t] -> RP_Variable (Some v, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "rp:constructor"; Sexpr.Sident v; t] -> RP_Constructor (v, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "rp:apply"; p1; p2; t] -> RP_Apply ((pattern_of_sexpr p1, pattern_of_sexpr p2), TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "rp:and"; p1; p2; t] -> RP_And ((pattern_of_sexpr p1, pattern_of_sexpr p2), TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "rp:or"; p1; p2; t] -> RP_Or ((pattern_of_sexpr p1, pattern_of_sexpr p2), TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "rp:not"; p; t] -> RP_Not ((pattern_of_sexpr p), TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr (Sexpr.Sident "rp:tuple" :: t :: ps) ->  RP_Tuple (List.map pattern_of_sexpr ps, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr (Sexpr.Sident "rp:vector" :: t :: ps) ->  RP_Vector (List.map pattern_of_sexpr ps, TypingType.oType_of_sexpr t)
  | _ -> invalid_arg "pattern_of_sexpr"

let gather (v, t, e) e' =
  R_Let ((v, t), e, e')

let rec to_sexpr = function
  | R_Constant (l, t) -> Sexpr.Sexpr [Sexpr.Sident "r:constant"; Syntax.lit_to_sexpr l; TypingType.oType_to_sexpr t]
  | R_Variable (v, t) -> Sexpr.Sexpr [Sexpr.Sident "r:var"; Sexpr.Sident v; TypingType.oType_to_sexpr t]
  | R_Fun((v, t), e) -> 
    let rec fun_flatten vs = function
      | R_Fun((v, t), e) ->  fun_flatten (R_Variable(v, t) :: vs) e
      | e -> (List.rev vs), e
    in
    let vs, e' = fun_flatten [R_Variable(v, t)] e in
    Sexpr.Sexpr [Sexpr.Sident "r:fun"; Sexpr.Sexpr(List.map to_sexpr vs); to_sexpr e']
  | R_Apply(e1, e2) -> 
    let rec apply_flatten = function
      | R_Apply(e1, e2) ->  e1 :: apply_flatten e2
      | e -> [e]
    in
    Sexpr.Sexpr (Sexpr.Sident "r:apply" ::  to_sexpr e1 :: List.map to_sexpr (apply_flatten e2))
  | R_Tuple (es, t) -> Sexpr.Sexpr (Sexpr.Sident "r:tuple" :: TypingType.oType_to_sexpr t :: List.map to_sexpr es)
  | R_Vector (es, t) -> Sexpr.Sexpr (Sexpr.Sident "r:vector" :: TypingType.oType_to_sexpr t :: List.map to_sexpr es)
  | R_If (e1, e2, e3) -> Sexpr.Sexpr (Sexpr.Sident "r:if" :: List.map to_sexpr [e1; e2; e3])
  | R_Let ((v, t), e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "r:let"; to_sexpr (R_Variable(v, t)); to_sexpr e1; to_sexpr e2]
  | R_Fix ((v, t), e, t') -> Sexpr.Sexpr [Sexpr.Sident "r:fix"; to_sexpr (R_Variable(v, t)); to_sexpr e; oType_to_sexpr t']
  | R_Match(e, cls) ->
    let f = function
      | p, None, e ->
        Sexpr.Sexpr [pattern_to_sexpr p; to_sexpr e]
      | p, Some g, e ->
        Sexpr.Sexpr [pattern_to_sexpr p; Sexpr.Sident ":"; to_sexpr g; to_sexpr e]
    in
    Sexpr.tagged_sexpr "r:match" (List.map f cls)
  | R_External (v, t) -> Sexpr.Sexpr [Sexpr.Sident "r:external-symbol"; Sexpr.Sident v; TypingType.oType_to_sexpr t]
and pattern_to_sexpr = function
  | RP_Constant (lit, t) -> Sexpr.tagged_sexpr "rp:constant" [Syntax.lit_to_sexpr lit; TypingType.oType_to_sexpr t]
  | RP_Variable (None, t) -> Sexpr.tagged_sexpr "rp:any" [TypingType.oType_to_sexpr t]
  | RP_Variable (Some v, t) -> Sexpr.tagged_sexpr "rp:var" [Sexpr.Sident v; TypingType.oType_to_sexpr t]
  | RP_Constructor (v, t) -> Sexpr.tagged_sexpr "rp:constructor" [Sexpr.Sident v; TypingType.oType_to_sexpr t]
  | RP_Apply ((p1, p2), t) -> Sexpr.tagged_sexpr "rp:apply" [pattern_to_sexpr p1; pattern_to_sexpr p2; TypingType.oType_to_sexpr t]
  | RP_And ((p1, p2), t) -> Sexpr.tagged_sexpr "rp:and" [pattern_to_sexpr p1; pattern_to_sexpr p2; TypingType.oType_to_sexpr t]
  | RP_Or ((p1, p2), t) -> Sexpr.tagged_sexpr "rp:or" [pattern_to_sexpr p1; pattern_to_sexpr p2; TypingType.oType_to_sexpr t]
  | RP_Not (p, t) -> Sexpr.tagged_sexpr "rp:not" [pattern_to_sexpr p; TypingType.oType_to_sexpr t]
  | RP_Tuple (ps, t) -> Sexpr.tagged_sexpr "rp:tuple" (TypingType.oType_to_sexpr t :: List.map pattern_to_sexpr ps)
  | RP_Vector (ps, t) -> Sexpr.tagged_sexpr "rp:vector" (TypingType.oType_to_sexpr t :: List.map pattern_to_sexpr ps)


let rec result_type = function
  | R_Constant (l, t) -> t
  | R_Variable (v, t) -> t
  | R_Fun((v, t), e) -> TypingType.O_Fun (t, result_type e)
  | R_Apply(e1, e2) ->
    begin match result_type e1 with
      | TypingType.O_Fun(ft, tt) -> tt
      | _ -> invalid_arg ""
    end
  | R_Tuple (es, t) -> t
  | R_Vector (es, t) -> t
  | R_If (e1, e2, e3) -> result_type e2
  | R_Let ((v, t), e1, e2) -> result_type e2
  | R_Fix ((v, t), e, t') -> t
  | R_External (v, t) -> t
  | R_Match (_, (_, _, e) :: _) ->
    result_type e
  | R_Match (_,  _) ->
    failwith "Empty clause."

let pattern_type = function
  | RP_Constant (_, t) 
  | RP_Variable (_, t) 
  | RP_Constructor (_, t)
  | RP_Apply (_, t)
  | RP_And (_, t)
  | RP_Or (_, t)
  | RP_Not (_, t)
  | RP_Tuple (_, t)
  | RP_Vector (_, t) -> t
