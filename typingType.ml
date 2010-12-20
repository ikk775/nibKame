type typeVar = Id.t
type typeConst = Id.t
type exprVar = Id.t

module TypeVarSet = Set.Make(String)

type oType =
  | O_Constant of Type.t
  | O_Variable of typeVar
  | O_Tuple of oType list
  | O_Vector of oType
  | O_Variant of oType * oType
  | O_Fun of oType * oType
  | O_Ref of oType

type typeScheme =
  | OType of oType
  | QType of typeVar list * typeScheme

type t = typeScheme

type typeEnv =
  | TypeEnv of (exprVar * typeScheme) list

type substitution =
  | Substitution of typeVar * oType

let genTypeVarNum = ref 0

let genTypeVar () =
  genTypeVarNum := !genTypeVarNum + 1;
  O_Variable (Format.sprintf "$ot:%d" !genTypeVarNum)

let rec genTypeVars n =
  if n > 0
  then genTypeVar () :: genTypeVars (n - 1)
  else []

let getOTypeVariableName = function
  | O_Variable v -> v
  | _ -> invalid_arg "expected O_Variable"

let rec removeQuantifier = function
  | OType ot -> ot
  | QType(_, ts) -> removeQuantifier ts

let rec bindedVars = function
  | OType t -> []
  | QType(qv, ts) -> MyUtil.List.unique (List.append qv (bindedVars ts))

let typeVars ot =
  let rec typeVars_sub = function
    | O_Constant _ -> TypeVarSet.empty
    | O_Variable tv -> TypeVarSet.singleton tv
    | O_Tuple(ts) -> List.fold_left TypeVarSet.union TypeVarSet.empty (List.map typeVars_sub ts)
    | O_Vector(t) -> typeVars_sub t
    | O_Variant(t1, t2) -> TypeVarSet.union (typeVars_sub t1) (typeVars_sub t2)
    | O_Fun(t1, t2) -> TypeVarSet.union (typeVars_sub t1) (typeVars_sub t2)
    | O_Ref(t) -> typeVars_sub t
  in
  TypeVarSet.elements (typeVars_sub ot)

let occur tv t =
  List.mem tv (typeVars t)
 
let rec freeTypeVars = function
  | OType t -> typeVars t
  | QType(qv, ts) -> List.filter (fun x -> not (List.mem x qv)) (freeTypeVars ts)

let rec normalizeTypeScheme ts =
  QType(bindedVars ts, OType (removeQuantifier ts))

let rec freeTypeVarsEnv = function
  | TypeEnv l -> TypeVarSet.elements (List.fold_right TypeVarSet.add (List.fold_left (fun a b -> List.append a (freeTypeVars (snd b))) [] l) TypeVarSet.empty)

let rec domain = function
  | [] -> []
  | Substitution(ftv, _) :: ss -> ftv :: domain ss

let rec targetVars ss =
  List.map (fun x -> O_Variable(x)) (domain ss)

let domainRestrict ss dom =
  List.filter (function Substitution(ftv, _) as s -> List.mem ftv dom) ss

let domainDiff ss dom =
  domainRestrict ss (MyUtil.List.setDiff (domain ss) dom)

let rec substitute ss tv =
  match ss, tv with 
    | [], ot -> ot
    | _, O_Constant tc -> O_Constant tc
    | Substitution(ftv, tot) :: ss, O_Variable tv  -> 
      if ftv = tv then tot
      else substitute ss (O_Variable tv)
    | ss, O_Tuple ts -> O_Tuple(List.map (substitute ss) ts)
    | ss, O_Vector t -> O_Vector(substitute ss t)
    | ss, O_Variant (ftv, ttv) -> O_Variant(substitute ss ftv, substitute ss ttv)
    | ss, O_Fun (ftv, ttv) -> O_Fun(substitute ss ftv, substitute ss ttv)
    | ss, O_Ref t -> O_Ref(substitute ss t)

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

let compositeSubsts sss =
  List.fold_right composite sss []

let rec supp ss =
  List.filter (fun tv -> tv = substitute ss tv) (targetVars ss)
  
let rec eq_subst ss1 ss2 =
  List.for_all (fun a -> substitute ss1 a = substitute ss2 a) (List.append (supp ss1) (supp ss2))
  
let rec substituteTs ss ts =
  match ss, ts with
    | ss, OType ot -> OType (substitute ss ot)
    | ss, QType (vs, ts) -> 
      let tvs = genTypeVars(List.length vs) in
      let tv_to_c = function O_Variable tv -> tv | _ -> invalid_arg "expected O_Variable" in
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

let addEnv = function TypeEnv envList -> fun tv ts -> 
  TypeEnv((tv, ts) :: List.remove_assoc tv envList)
  
let clos env ts =
  let freeVars = MyUtil.List.setDiff (freeTypeVars ts) (freeTypeVarsEnv env) in
  QType(freeVars, ts)

let substituteEqnPair ss pair =
  match pair with
    | x, y -> substitute ss x, substitute ss y
 
let substituteEqnPairs ss pairs =
  List.map (substituteEqnPair ss) pairs

exception Unification_Failure of (oType * oType) list * substitution list

let rec unify_u eqns substs =
  match eqns with
    | [] -> substs
    | (s, t) :: eqns' when s = t -> unify_u eqns' substs
    | (O_Fun(t1, e1) , O_Fun(t2, e2)):: eqns' -> unify_u ((t1, t2) :: (e1, e2) :: eqns') substs
    | (O_Variant(t1, e1) , O_Variant(t2, e2)):: eqns' -> unify_u ((t1, t2) :: (e1, e2) :: eqns') substs
    | (O_Vector(t1) , O_Vector(t2)):: eqns' -> unify_u ((t1, t2) :: eqns') substs
    | (O_Ref(t1) , O_Ref(t2)):: eqns' -> unify_u ((t1, t2) :: eqns') substs
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

let renew : oType -> oType -> substitution list = fun older newer -> 
  let ss = unify older newer in
  let otvs = typeVars older in
  let f = function
    | Substitution (t1, O_Variable t2) when List.mem t1 otvs -> Substitution (t1, O_Variable t2)
    | Substitution (t1, O_Variable t2) when List.mem t2 otvs -> Substitution (t2, O_Variable t1)
    | _ as a -> a
  in
  domainRestrict (List.map f ss) otvs

let oType_of_type : Type.t -> oType = fun x ->
  let rec of_type = function
    | Type.Unit as c -> O_Constant c
    | Type.Bool as c -> O_Constant c
    | Type.Int as c -> O_Constant c
    | Type.Float as c -> O_Constant c
    | Type.Char as c -> O_Constant c
    | Type.Fun (ts , t') ->
      begin match ts with
        | [t] -> O_Fun(of_type t, of_type t')
        | t :: ts' -> O_Fun(of_type t, of_type (Type.Fun (ts', t')))
        | [] -> invalid_arg "Type.Fun needs one or more argument types."
      end
    | Type.Tuple ts -> O_Tuple (List.map of_type ts)
    | Type.Array t -> O_Variant (of_type t, of_type (Type.Variant "array"))
    | Type.List t -> O_Variant (of_type t, of_type (Type.Variant "list"))
    | (Type.Variant x) as c -> O_Constant c
    | Type.Var x -> O_Variable x
    | Type.Ref t -> O_Ref (of_type t)
  in
  of_type x

let oType_to_type : oType -> Type.t = fun x ->
  let rec to_type = function
    | O_Constant Type.Unit -> Type.Unit
    | O_Constant Type.Bool -> Type.Bool
    | O_Constant Type.Int -> Type.Int
    | O_Constant Type.Char -> Type.Char
    | O_Constant Type.Float -> Type.Float
    | O_Fun (t1 , t2) -> Type.Fun ([to_type t1], to_type t2)
    | O_Tuple (ts) -> Type.Tuple (List.map to_type ts)
    | O_Variant (t, O_Constant (Type.Variant "array")) -> Type.Array(to_type t)
    | O_Variant (t, O_Constant (Type.Variant "list")) -> Type.List (to_type t)
    | O_Constant (Type.Variant x) -> Type.Variant x
    | O_Variable x -> Type.Var x
    | O_Ref t -> Type.Ref (to_type t)
    | _ -> invalid_arg ""
  in
  to_type x

let oType_to_mType : oType -> Type.mType = fun x ->
  Type.to_mt (oType_to_type x)

let rec oType_to_sexpr = function
  | O_Constant t -> Sexpr.Sexpr [Sexpr.Sident "ot:constant"; Type.to_sexpr t]
  | O_Variable t -> Sexpr.Sexpr [Sexpr.Sident "ot:var"; Sexpr.Sident t]
  | O_Tuple ts -> Sexpr.Sexpr (Sexpr.Sident "ot:tuple" :: (List.map oType_to_sexpr ts))
  | O_Vector t -> Sexpr.Sexpr [Sexpr.Sident "ot:vector"; oType_to_sexpr t]
  | O_Variant(t1, t2) ->
    let rec variant_flatten = function
      | O_Variant(t1, t2) ->  t1 :: variant_flatten t2
      | t -> [t]
    in
    Sexpr.Sexpr (Sexpr.Sident "ot:variant" ::  oType_to_sexpr t1 :: List.map oType_to_sexpr (variant_flatten t2))
  | O_Fun(t1, t2) -> 
    let rec fun_flatten = function
      | O_Fun(t1, t2) ->  t1 :: fun_flatten t2
      | t -> [t]
    in
    Sexpr.Sexpr (Sexpr.Sident "ot:fun" ::  oType_to_sexpr t1 :: List.map oType_to_sexpr (fun_flatten t2))
  | O_Ref(t) -> Sexpr.Sexpr [Sexpr.Sident "ot:ref"; oType_to_sexpr t]

let rec oType_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "ot:constant"; t] -> O_Constant (Type.of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "ot:var"; Sexpr.Sident t] -> O_Variable t
  | Sexpr.Sexpr (Sexpr.Sident "ot:tuple" :: ts) -> O_Tuple (List.map oType_of_sexpr ts)
  | Sexpr.Sexpr [Sexpr.Sident "ot:vector"; t] -> O_Vector (oType_of_sexpr t)
  | Sexpr.Sexpr (Sexpr.Sident "ot:variant" :: t1 :: t2 :: ts) -> 
    let rec variant_nest = function
      | t1 :: t2 :: []->  O_Variant(oType_of_sexpr t1, oType_of_sexpr t2)
      | t :: ts -> O_Variant(oType_of_sexpr t, variant_nest ts)
      | _ -> invalid_arg "unexpected token."
    in
    variant_nest (t1 :: t2 :: ts)
  | Sexpr.Sexpr (Sexpr.Sident "ot:fun" :: t1 :: t2 :: ts) -> 
    let rec fun_nest = function
      | t1 :: t2 :: []->  O_Fun(oType_of_sexpr t1, oType_of_sexpr t2)
      | t :: ts -> O_Fun(oType_of_sexpr t, fun_nest ts)
      | _ -> invalid_arg "unexpected token."
    in
    fun_nest (t1 :: t2 :: ts)
  | Sexpr.Sexpr [Sexpr.Sident "ot:ref"; t] -> O_Ref (oType_of_sexpr t)
  | _ -> invalid_arg "unexpected token."

let rec typeScheme_to_sexpr = function
  | OType ot -> Sexpr.Sexpr [Sexpr.Sident "ts:type"; oType_to_sexpr ot]
  | QType (vs, ts) -> Sexpr.Sexpr [Sexpr.Sident "ts:forall"; Sexpr.Sexpr (List.map (fun v -> Sexpr.Sident v) vs); typeScheme_to_sexpr ts]

let rec typeScheme_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "ts:type"; ot] -> OType (oType_of_sexpr ot)
  | Sexpr.Sexpr [Sexpr.Sident "ts:forall"; Sexpr.Sexpr vs; ts] -> 
    let id_to_str = function
      | Sexpr.Sident x -> x
      | _ -> invalid_arg "unexpected token."
    in
    let vs' = List.map id_to_str vs in
    QType (vs', typeScheme_of_sexpr ts)
  | _ -> invalid_arg "unexpected token."

let rec typeEnv_to_sexpr = function
  | TypeEnv eqs ->
    let eq_to_sexpr = function
      | v, ts -> Sexpr.Sexpr [Sexpr.Sident v; typeScheme_to_sexpr ts]
    in
    Sexpr.Sexpr (Sexpr.Sident "te:env" :: List.map eq_to_sexpr eqs)

let rec typeEnv_of_sexpr = function
  | Sexpr.Sexpr (Sexpr.Sident "te:env" :: eqs) -> 
    let eq_of_sexpr = function
      | Sexpr.Sexpr [Sexpr.Sident v; ts] -> v, typeScheme_of_sexpr ts
      | _ -> invalid_arg "unexpected token."
    in
    List.map eq_of_sexpr eqs
  | _ -> invalid_arg "unexpected token."
    
let rec substitution_to_sexpr = function
  | Substitution(tv, ot) -> Sexpr.Sexpr (List.map oType_to_sexpr [O_Variable tv; ot])

let rec substitution_of_sexpr = function
  | Sexpr.Sexpr[tv; ot] -> Substitution (getOTypeVariableName(oType_of_sexpr tv), oType_of_sexpr ot)
  | _ -> invalid_arg "unexpected token."

let rec substitutions_to_sexpr = function
  | ss -> Sexpr.Sexpr (List.map substitution_to_sexpr ss)

let rec substitutions_of_sexpr = function
  | Sexpr.Sexpr (ss) -> 
    List.map substitution_of_sexpr ss
  | _ -> invalid_arg "unexpected token."

