type typeVar = Id.t
type typeConst = Id.t
type exprVar = Id.t
type exprConst = Id.t

type oType =
  | O_Constant of typeConst
  | O_Variable of typeVar
  | O_Fun of oType * oType

type typeScheme =
  | OType of oType
  | QType of typeVar list * typeScheme

type typeEnv =
  | TypeEnv of (exprVar * typeScheme) list

type expr =
  | E_Constant of exprConst
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Let of exprVar * expr * expr
  | E_Fix of exprVar * expr
  | E_Type of expr * oType

type substitution =
  | Substitution of typeVar * oType

let getConstantType = function
  | E_Constant c -> O_Constant c
  | _ -> invalid_arg "expected type E_Constant"

let getVariableType env expr =
  match env, expr with
    | TypeEnv envList, E_Variable v -> List.assoc v envList
    | _ -> invalid_arg "expected type E_Variable"

let genTypeVarNum = ref 0

let genTypeVar () =
  genTypeVarNum := !genTypeVarNum + 1;
  O_Variable ("$" ^ string_of_int !genTypeVarNum)

let rec genTypeVars n =
  if n > 0
  then genTypeVar () :: genTypeVars (n - 1)
  else []

let rec removeQuantifier = function
  | OType ot -> ot
  | QType(_, ts) -> removeQuantifier ts

let rec typeVars  = function
  | O_Constant _ -> []
  | O_Variable tv -> [tv]
  | O_Fun(t1, t2) -> ExtList.List.unique(List.append (typeVars t1) (typeVars t2))

let occur tv t =
  List.mem tv (typeVars t)
 
let rec freeTypeVars = function
  | OType t -> typeVars t
  | QType(qv, ts) -> List.filter (fun x -> not (List.mem x qv)) (freeTypeVars ts)

let rec freeTypeVarsEnv = function
  | TypeEnv l -> ExtList.List.unique (List.fold_left (fun a b -> List.append a (freeTypeVars (snd b))) [] l)

let rec targetVars = function
  | [] -> []
  | Substitution(ftv, _) :: ss -> O_Variable(ftv) :: targetVars ss

let rec substitute ss tv =
  match ss, tv with 
    | [], ot -> ot
    | _, O_Constant tc -> O_Constant tc
    | Substitution(ftv, tot) :: ss, O_Variable tv  -> 
      if ftv = tv then tot
      else substitute ss (O_Variable tv)
    | ss, O_Fun (ftv, ttv) -> O_Fun(substitute ss ftv, substitute ss ttv)

let rec composite (xs:substitution list) (ys:substitution list) =
  let subst_tot x ys = (List.map (fun y -> match y with Substitution(ftv, tot) -> Substitution (ftv ,substitute [x] tot)) ys) in
  match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, ys ->
      if (List.exists (fun y -> 
        match x, y with
          | Substitution(ftvx, _), Substitution (ftvy, _) -> ftvx = ftvy) ys)
      then composite xs (subst_tot x ys)
      else composite xs (x :: subst_tot x ys)

let rec supp ss =
  List.filter (fun tv -> tv = substitute ss tv) (targetVars ss)
  
let rec eq_subst ss1 ss2 =
  List.for_all (fun a -> substitute ss1 a = substitute ss2 a) (List.append (supp ss1) (supp ss2))
  
let rec substituteTs ss ts =
  match ss, ts with
    | ss, OType ot -> OType (substitute ss ot)
    | ss, QType (vs, ts) -> 
      let tvs = genTypeVars(List.length vs) in
      let tv_to_c = function O_Variable tv -> tv in
      let tvs_to_cs tvs = List.map tv_to_c tvs in
      let tvsc = tvs_to_cs tvs in
      let gen_subst ftvs ttvs =
        match ftvs, ttvs with
          | ftvc, ttv -> Substitution ( ftvc, ttv)
      in
      let middle_subst = List.map2 gen_subst vs tvs in
      match ts with
        | OType ot -> QType(tvsc, OType (substitute ss (substitute middle_subst ot)))
        | qt -> QType(tvsc, substituteTs ss (substituteTs middle_subst qt))

let substituteEnv ss env =
  match env with
    | TypeEnv envList -> TypeEnv(List.map (function ftw, tts -> ftw, substituteTs ss tts) envList)

let substituteEqnPair ss pair =
  match pair with
    | x, y -> substitute ss x, substitute ss y
 
let substituteEqnPairs ss pairs =
  List.map (substituteEqnPair ss) pairs

let addEnv = function TypeEnv envList -> fun tv ts -> 
  TypeEnv((tv, ts) :: List.remove_assoc tv envList)
  
let clos env ts =
  let freeVars = MyUtil.List.setDiff (freeTypeVars ts) (freeTypeVarsEnv env) in
  QType(freeVars, ts)

exception Unification_Failure of (oType * oType) list * substitution list

let rec unify_u eqns substs =
  match eqns with
    | [] -> substs
    | (s, t) :: eqns' when s = t -> unify_u eqns' substs
    | (O_Fun(t1, e1) , O_Fun(t2, e2)):: eqns' -> unify_u ((t1, t2) :: (e1, e2) :: eqns') substs
    | (O_Constant c1, O_Constant c2) :: eqns' -> raise (Unification_Failure(eqns, substs))
    | (t, O_Variable v) :: eqns' when (match t with O_Variable _ -> false | _ -> true) -> 
      unify_u ((O_Variable v, t) :: eqns') substs
    | (O_Variable v, t) :: eqns' when occur v t && (O_Variable v) <> t ->
      raise (Unification_Failure(eqns, substs))
    | (O_Variable v, t) :: eqns' when not (occur v t) ->
      let substs' = [Substitution (v, t)] in
      let eqns'' = substituteEqnPairs substs' eqns' in
      let substs'' = composite substs' substs in
      unify_u eqns'' substs''
    | _ -> raise (Unification_Failure(eqns, substs))

let unify: oType -> oType -> substitution list = fun t1 t2 -> 
  unify_u [(t1, t2)] []

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
      [], t, E_Type(expr, t)
    | E_Fun(v, expr) -> 
      let b = genTypeVar () in
      let s1, t1, expr' = w (addEnv env v (OType b)) expr in
      let t2 = O_Fun(substitute s1 b, t1) in
      s1, t2, E_Type(E_Fun(v, expr'), t2)
    | E_Apply(e1, e2) -> 
      let b = genTypeVar () in
      let s1, t1, e1' = w env e1 in
      let s2, t2, e2' = w (substituteEnv s1 env) e2 in
      let s3 = unify (substitute s2 t1) (O_Fun(t2, b)) in
      let t = substitute s3 b in
      composite s3 (composite s2 s1), t, E_Type(E_Apply(e1', e2'), t)
    | E_Let(v, e1, e2)  -> 
      let s1, t1, e1' = w env e1 in
      let s1env = substituteEnv s1 env in
      let s2, t2, e2' = w (addEnv s1env v (clos s1env (OType t1))) e2 in
      composite s2 s1, t2, E_Type(E_Let(v, e1', e2'), t2)
    | E_Fix(f, E_Fun(x, e)) -> 
      let b = genTypeVar () in
      let s1, t1, e' = w (addEnv env f (OType b)) (E_Fun(x, e)) in
      let s2 = unify (substitute s1 b) t1 in
      let t2 = substitute s2 t1 in
      composite s2 s1, t2, E_Type(E_Fix(f, E_Type(E_Fun(x, e'), t1)), t2)
    | _ -> invalid_arg "Invalid expr."

  