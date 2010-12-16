open MyUtil
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
  | R_If of result * result * result
  | R_Let of (resultVar * TypingType.oType) * result * result
  | R_Fix of (resultVar * TypingType.oType) * result * TypingType.oType

let rec bindedVars : result -> (Id.t * TypingType.oType) list = function
  | R_Constant (l, t) -> []
  | R_Variable (v, t) -> []
  | R_Fun((v, t), e) -> (v, t) :: bindedVars e
  | R_Apply(e1, e2) -> List.append (bindedVars e1) (bindedVars e2)
  | R_Tuple (es, t) -> List.concat (List.map bindedVars es)
  | R_Vector (es, t) -> List.concat (List.map bindedVars es)
  | R_If (e1, e2, e3) -> List.concat (List.map bindedVars [e1; e2; e3])
  | R_Let ((v, t), e1, e2) -> (v, t) :: List.concat (List.map bindedVars [e1; e2])
  | R_Fix ((v, t), e, t') -> (v, t) :: bindedVars e

let rec freeVars : result -> (Id.t * TypingType.oType) list = function
  | R_Constant (l, t) -> []
  | R_Variable (v, t) -> [(v, t)]
  | R_Fun((v, t), e) -> List.remove_assoc v (freeVars e)
  | R_Apply(e1, e2) -> List.unique (List.append (freeVars e1) (freeVars e2))
  | R_Tuple (es, t) -> List.unique (List.concat (List.map freeVars es))
  | R_Vector (es, t) -> List.unique (List.concat (List.map freeVars es))
  | R_If (e1, e2, e3) -> List.unique (List.concat (List.map freeVars [e1; e2; e3]))
  | R_Let ((v, t), e1, e2) -> List.remove_assoc v(List.unique (List.concat (List.map freeVars [e1; e2]))) 
  | R_Fix ((v, t), e, t') -> List.remove_assoc v (freeVars e)

let rec typeVars : result -> Id.t list = fun r -> 
  let ftv t = TypingType.freeTypeVars (TypingType.OType t) in
  let rec g = function
    | R_Constant (l, t) -> []
    | R_Variable (v, t) -> [ftv t]
    | R_Fun((v, t), e) -> List.unique (ftv t :: g e)
    | R_Apply(e1, e2) -> List.unique (List.append (g e1) (g e2))
    | R_Tuple (es, t) -> List.unique (List.concat (List.map g es))
    | R_Vector (es, t) -> List.unique (List.concat (List.map g es))
    | R_If (e1, e2, e3) -> List.unique (List.concat (List.map g [e1; e2; e3]))
    | R_Let ((v, t), e1, e2) -> List.unique (ftv t :: (List.concat (List.map g [e1; e2]))) 
    | R_Fix ((v, t), e, t') -> List.unique (ftv t :: g e)
  in
  List.concat (g r)

let rec resultFreeTypeVars : (Id.t * TypingType.oType) list -> Id.t list = fun vts -> 
  List.concat (List.map (function x, t -> TypingType.freeTypeVars (TypingType.OType t)) vts)

let rec valueRestrict : result -> TypingType.oType -> TypingType.typeScheme = fun r t -> 
  let rec f = function
    | R_Fun ((v, t), r) -> t :: f r
    | _ -> []
  in
  let ots = f r in
  let rec ftv = MyUtil.List.unique (List.concat (List.map (fun ot -> TypingType.freeTypeVars (TypingType.OType ot)) ots)) in
  TypingType.QType (ftv, TypingType.OType t)

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
    | R_If(e1, e2, e3) -> R_If(subst e1, subst e2, subst e3)
    | R_Let((v, t), e1, e2) -> R_Let((v, tsubst t), subst e1, subst e2)
    | R_Fix((f, t), e, t') -> R_Fix((f, tsubst t), subst e, tsubst t')

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

let rec w env expr =
  match expr with
    | E_Constant c ->
      let t = getConstantType (E_Constant c) in
      [], t, R_Constant(c, t)
    | E_Variable v ->
      let ts = getVariableType env (E_Variable v) in
      let freeTypeVarsTs = TypingType.freeTypeVars ts in
      let newTypeVars = genTypeVars (List.length freeTypeVarsTs) in
      let subst = List.map (function x, y -> Substitution(x,y)) (List.combine freeTypeVarsTs newTypeVars) in
      let t = substitute subst (removeQuantifier ts) in
      subst, t, R_Variable(v, t)
    | E_Fun(v, expr) -> 
      let b = genTypeVar () in
      let s1, t1, expr' = w (TypingExpr.addEnv env v (OType b)) expr in
      let bt = substitute s1 b in
      let t2 = O_Fun(bt, t1) in
      s1, t2, R_Fun((v, bt), expr')
    | E_Apply(e1, e2) -> 
      let b = genTypeVar () in
      let s1, t1, e1' = w env e1 in
      let s2, t2, e2' = w (TypingExpr.substituteEnv s1 env) e2 in
      let s3 = unify (substitute s2 t1) (O_Fun(t2, b)) in
      let t = substitute s3 b in
      composite s3 (composite s2 s1), t, R_Apply(e1', e2')
    | E_Tuple(es) -> 
      let rec w_sub env es ss ts e's =
        match es with
          | [] -> ss, ts, e's
          | e :: es' -> 
            let s, t, e' = w env e in
            let env' = (TypingExpr.substituteEnv s env) in
            w_sub env' es' (s :: ss) (t :: ts) (e' :: e's)
      in
      let w_results = w_sub env es [] [] [] in
      let ss, ts, e's = w_results in
      let e's' = List.rev e's in
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
          u_sub (TypingExpr.substituteEnv ss' env) (composite ss' ss) (substitute ss' t2') ts)
      in
      let b = genTypeVar () in
      let env', ss', t' = u_sub env ss b ts in
      let t'' = O_Vector t' in
      ss', t'', R_Vector(e'', t'')
    | E_If(e1, e2, e3)  -> 
      let s1, t1, e1' = w env e1 in
      let env' = (TypingExpr.substituteEnv s1 env) in
      let s2, t2, e2' = w env' e2 in
      let env'' = (TypingExpr.substituteEnv s2 env') in
      let s3, t3, e3' = w env'' e3 in
      let s4 = unify t1 (O_Constant Type.Bool) in
      let s5 = unify t2 t3 in
      let t = substitute s5 t2 in
      compositeSubsts [s5;s4;s3;s2;s1], t, R_If(e1', e2', e3')
    | E_Let(v, e1, e2)  -> 
      let s1, t1, e1' = w env e1 in
      let s1env = TypingExpr.substituteEnv s1 env in
      let s2, t2, e2' = w (TypingExpr.addEnv s1env v (TypingExpr.clos s1env (OType t1))) e2 in
      composite s2 s1, t2, R_Let((v, t1), e1', e2')
    | E_Fix(f, E_Fun(x, e)) -> 
      let b = genTypeVar () in
      let s1, t1, e' = w (TypingExpr.addEnv env f (OType b)) (E_Fun(x, e)) in
      let s2 = unify (substitute s1 b) t1 in
      let t2 = substitute s2 t1 in
      let bt = substitute s2 b in
      composite s2 s1, t2, R_Fix((f, bt), e', t2)
    | _ -> invalid_arg "Invalid expr."

let typing_with_subst env expr =
  let ss, t, expr' = w env expr in
  let expr'' = substituteResultType ss expr' in
  valueRestrict expr'' t, expr'', ss

let typing env expr =
  let ss, t, expr' = w env expr in
  let expr'' = substituteResultType ss expr' in
  valueRestrict expr'' t, expr''
  
let rec result_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "r:constant"; l; t] -> R_Constant (Syntax.lit_of_sexpr l, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "r:var"; Sexpr.Sident v; t] -> R_Variable (v, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "r:fun"; Sexpr.Sexpr vs; e] -> 
    let fun_nest e' vs =
      let rec fun_nest_sub = function
        | [v] -> 
          (match result_of_sexpr v with
            | R_Variable(v, t) -> R_Fun((v, t), result_of_sexpr e')
            | _ -> invalid_arg "unexpected token.")
        | v :: vs ->
          (match result_of_sexpr v with
            | R_Variable(v, t) -> R_Fun((v, t), fun_nest_sub vs)
            | _ -> invalid_arg "unexpected token.")
        | _ -> invalid_arg "unexpected token."
      in
      fun_nest_sub vs
    in
    fun_nest e vs
  | Sexpr.Sexpr (Sexpr.Sident "r:apply" :: e1 :: e2 :: es) -> 
    let rec apply_nest = function
      | e1 :: e2 :: []->  R_Apply(result_of_sexpr e1, result_of_sexpr e2)
      | e :: es -> R_Apply(result_of_sexpr e, apply_nest es)
      | _ -> invalid_arg "unexpected token."
    in
    apply_nest (e1 :: e2 :: es)
  | Sexpr.Sexpr (Sexpr.Sident "r:tuple" :: t :: es) -> R_Tuple(List.map result_of_sexpr es, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr (Sexpr.Sident "r:vector" :: t :: es) -> R_Vector(List.map result_of_sexpr es, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "r:if" ; e1 ; e2; e3] -> R_If(result_of_sexpr e1, result_of_sexpr e2, result_of_sexpr e3)
  | Sexpr.Sexpr [Sexpr.Sident "r:let"; v; e1; e2] -> 
    (match result_of_sexpr v with
      | R_Variable(v, t) -> R_Let((v, t), result_of_sexpr e1, result_of_sexpr e2)
      | _ -> invalid_arg "unexpected token.")
  | Sexpr.Sexpr [Sexpr.Sident "r:fix"; v; e; t'] -> 
    (match result_of_sexpr v with
      | R_Variable(v, t) -> R_Fix((v, t), result_of_sexpr e, TypingType.oType_of_sexpr t')
      | _ -> invalid_arg "unexpected token.")
  | _ -> invalid_arg "unexpected token."

let rec result_to_sexpr = function
  | R_Constant (l, t) -> Sexpr.Sexpr [Sexpr.Sident "r:constant"; Syntax.lit_to_sexpr l; TypingType.oType_to_sexpr t]
  | R_Variable (v, t) -> Sexpr.Sexpr [Sexpr.Sident "r:var"; Sexpr.Sident v; TypingType.oType_to_sexpr t]
  | R_Fun((v, t), e) -> 
    let rec fun_flatten vs = function
      | R_Fun((v, t), e) ->  fun_flatten (R_Variable(v, t) :: vs) e
      | e -> (List.rev vs), e
    in
    let vs, e' = fun_flatten [R_Variable(v, t)] e in
    Sexpr.Sexpr [Sexpr.Sident "r:fun"; Sexpr.Sexpr(List.map result_to_sexpr vs); result_to_sexpr e']
  | R_Apply(e1, e2) -> 
    let rec apply_flatten = function
      | R_Apply(e1, e2) ->  e1 :: apply_flatten e2
      | e -> [e]
    in
    Sexpr.Sexpr (Sexpr.Sident "r:apply" ::  result_to_sexpr e1 :: List.map result_to_sexpr (apply_flatten e2))
  | R_Tuple (es, t) -> Sexpr.Sexpr (Sexpr.Sident "r:tuple" :: TypingType.oType_to_sexpr t :: List.map result_to_sexpr es)
  | R_Vector (es, t) -> Sexpr.Sexpr (Sexpr.Sident "r:vector" :: TypingType.oType_to_sexpr t :: List.map result_to_sexpr es)
  | R_If (e1, e2, e3) -> Sexpr.Sexpr (Sexpr.Sident "r:if" :: List.map result_to_sexpr [e1; e2; e3])
  | R_Let ((v, t), e1, e2) -> Sexpr.Sexpr [Sexpr.Sident "r:let"; result_to_sexpr (R_Variable(v, t)); result_to_sexpr e1; result_to_sexpr e2]
  | R_Fix ((v, t), e, t') -> Sexpr.Sexpr [Sexpr.Sident "r:fix"; result_to_sexpr (R_Variable(v, t)); result_to_sexpr e; oType_to_sexpr t']

let rec resultType = function
  | R_Constant (l, t) -> t
  | R_Variable (v, t) -> t
  | R_Fun((v, t), e) -> TypingType.O_Fun (t, resultType e)
  | R_Apply(e1, e2) ->
    begin match resultType e1 with
      | TypingType.O_Fun(ft, tt) -> tt
      | _ -> invalid_arg ""
    end
  | R_Tuple (es, t) -> t
  | R_Vector (es, t) -> t
  | R_If (e1, e2, e3) -> resultType e2
  | R_Let ((v, t), e1, e2) -> resultType e2
  | R_Fix ((v, t), e, t') -> t
