type exprVar = Id.t
type exprConst = Id.t

type expr =
  | E_Constant of Syntax.lit
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Tuple of expr list
  | E_If of expr * expr * expr
  | E_Let of exprVar * expr * expr
  | E_Fix of exprVar * expr
  | E_Type of expr * Type.oType
  | E_Declare of exprVar * Type.oType * expr

type exprEnv =
  | ExprEnv of (Id.t * Type.oType) list

let extExprEnv = ref []

let getExtExprEnv = !extExprEnv
let setExtExprEnv eenv =
  extExprEnv := eenv
let addExtExprEnv tp =
  extExprEnv := tp :: !extExprEnv

exception ExtFun_not_found of string
let getConstantType = function
  | E_Constant c ->
    (match c with
      | Syntax.Unit -> Type.O_Constant Type.Unit
      | Syntax.Bool _ -> Type.O_Constant Type.Bool
      | Syntax.Int _ -> Type.O_Constant Type.Int
      | Syntax.Float _ -> Type.O_Constant Type.Float
      | Syntax.Char _ -> Type.O_Constant Type.Char
      | Syntax.ExtFun f -> 
        (try
          List.assoc f !extExprEnv
        with
          | Not_found _ -> raise (ExtFun_not_found f)))
  | _ -> invalid_arg "expected type E_Constant"

let getVariableType env expr =
  match env, expr with
    | Type.TypeEnv envList, E_Variable v -> List.assoc v envList
    | _ -> invalid_arg "expected type E_Variable"

let rec substituteExpr ss expr =
  let subst = substituteExpr ss in
  match expr with
    | E_Variable v -> E_Variable v
    | E_Constant c -> E_Constant c
    | E_Fun(v, e) -> E_Fun(v, subst e)
    | E_Apply(e1, e2) -> E_Apply(subst e1, subst e2)
    | E_Tuple(es) -> E_Tuple(List.map subst es)
    | E_If(e1, e2, e3) -> E_If(subst e1, subst e2, subst e3)
    | E_Let(v, e1, e2) -> E_Let(v, subst e1, subst e2)
    | E_Fix(f, e) -> E_Fix(f, subst e)
    | E_Type(e, t) -> E_Type(subst e, Type.substitute ss t)
    | E_Declare(v, t, e) -> E_Declare(v, Type.substitute ss t, subst e)

let rec w (env:Type.typeEnv) expr =
  match expr with
    | E_Constant c ->
      let t = getConstantType (E_Constant c) in
      [], t, E_Type(expr, t)
    | E_Variable v ->
      let ts = getVariableType env (E_Variable v) in
      let freeTypeVarsTs = Type.freeTypeVars ts in
      let newTypeVars = Type.genTypeVars (List.length freeTypeVarsTs) in
      let subst = List.map (function x, y -> Type.Substitution(x,y)) (List.combine freeTypeVarsTs newTypeVars) in
      let t = Type.substitute subst (Type.removeQuantifier ts) in
      [], t, E_Type(expr, t)
    | E_Fun(v, expr) -> 
      let b = Type.genTypeVar () in
      let s1, t1, expr' = w (Type.addEnv env v (Type.OType b)) expr in
      let bt = Type.substitute s1 b in
      let t2 = Type.O_Fun(bt, t1) in
      s1, t2, E_Type(E_Fun(v, E_Declare(v, bt, expr')), t2)
    | E_Apply(e1, e2) -> 
      let b = Type.genTypeVar () in
      let s1, t1, e1' = w env e1 in
      let s2, t2, e2' = w (Type.substituteEnv s1 env) e2 in
      let s3 = Type.unify (Type.substitute s2 t1) (Type.O_Fun(t2, b)) in
      let t = Type.substitute s3 b in
      Type.composite s3 (Type.composite s2 s1), t, E_Type(E_Apply(e1', e2'), t)
    | E_Tuple(es) -> 
      let rec w_sub env es ss ts e's =
        match es with
          | [] -> ss, ts, e's
          | e :: es' -> 
            let s, t, e' = w env e in
            let env' = (Type.substituteEnv s env) in
            w_sub env' es' (s :: ss) (t :: ts) (e' :: e's)
      in
      let w_results = w_sub env es [] [] [] in
      let ss, ts, e's = w_results in
      let ts' = List.rev ts in
      let e's' = List.rev e's in
      let e's'' = List.map2 (fun e' t -> E_Type(e',t)) e's' ts' in
      let t = Type.O_Tuple(ts) in
      Type.compositeSubsts ss, t, E_Type(E_Tuple(e's''), t)
    | E_If(e1, e2, e3)  -> 
      let s1, t1, e1' = w env e1 in
      let env' = (Type.substituteEnv s1 env) in
      let s2, t2, e2' = w env' e2 in
      let env'' = (Type.substituteEnv s2 env') in
      let s3, t3, e3' = w env'' e3 in
      let s4 = Type.unify t1 (Type.O_Constant Type.Bool) in
      let s5 = Type.unify t2 t3 in
      let t = Type.substitute s5 t2 in
      Type.compositeSubsts [s5;s4;s3;s2;s1], t, E_Type(E_If(E_Type(e1',t1),E_Type(e2',t2),E_Type(e3',t3)),t)
    | E_Let(v, e1, e2)  -> 
      let s1, t1, e1' = w env e1 in
      let s1env = Type.substituteEnv s1 env in
      let s2, t2, e2' = w (Type.addEnv s1env v (Type.clos s1env (Type.OType t1))) e2 in
      Type.composite s2 s1, t2, E_Type(E_Let(v, e1', E_Declare(v, t1, e2')), t2)
    | E_Fix(f, E_Fun(x, e)) -> 
      let b = Type.genTypeVar () in
      let s1, t1, e' = w (Type.addEnv env f (Type.OType b)) (E_Fun(x, e)) in
      let s2 = Type.unify (Type.substitute s1 b) t1 in
      let t2 = Type.substitute s2 t1 in
      let bt = Type.substitute s2 b in
      Type.composite s2 s1, t2, E_Type(E_Fix(f, E_Declare(f, bt, E_Type(E_Fun(x, e'), t1))), t2)
    | _ -> invalid_arg "Invalid expr."

let typing env expr =
  let ss, t, expr' = w env expr in
  t, substituteExpr ss expr'
  