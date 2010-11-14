type t =
  | Unit
  | Bool
  | Int
  | Float
  | Char
  | Fun of t list * t
  | Tuple of t list
  | List of t
  | Array of t
  | Variant of Id.t
  | Var of t option ref
 
let gentype () = Var(ref None)

type typeVar = Id.t
type typeConst = Id.t
type exprVar = Id.t
type oType =
  | O_Constant of t
  | O_Variable of typeVar
  | O_Tuple of oType list
  | O_Variant of oType * oType
  | O_Fun of oType * oType

type typeScheme =
  | OType of oType
  | QType of typeVar list * typeScheme

type typeEnv =
  | TypeEnv of (exprVar * typeScheme) list

type substitution =
  | Substitution of typeVar * oType

let genTypeVarNum = ref 0

let genTypeVar () =
  genTypeVarNum := !genTypeVarNum + 1;
  O_Variable (Format.sprintf "#<type-variable:%d>" !genTypeVarNum)

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
  | O_Tuple(ts) -> ExtList.List.unique(List.concat(List.map typeVars ts))
  | O_Variant(t1, t2) -> ExtList.List.unique(List.append (typeVars t1) (typeVars t2))
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
    | ss, O_Tuple ts -> O_Tuple(List.map (substitute ss) ts)
    | ss, O_Variant (ftv, ttv) -> O_Variant(substitute ss ftv, substitute ss ttv)
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

let rec of_sexpr = function
  | Sexpr.Sident "unit" -> Unit
  | Sexpr.Sident "bool" -> Bool
  | Sexpr.Sident "int" -> Int
  | Sexpr.Sident "float" -> Float
  | Sexpr.Sident "char" -> Char
  | Sexpr.Sexpr [Sexpr.Sident "fun"; Sexpr.Sexpr t1s; t2] -> 
    Fun (List.map of_sexpr t1s, of_sexpr t2)
  | Sexpr.Sexpr (Sexpr.Sident "tuple" :: ts) -> 
    Tuple (List.map of_sexpr ts)
  | Sexpr.Sexpr [Sexpr.Sident "list"; t] -> 
    List (of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "array";  t] -> 
    Array (of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "variant"; Sexpr.Sident t] -> 
    Variant t
  | Sexpr.Sexpr (Sexpr.Sident "var" :: ts) -> 
    (match ts with
      | [] -> Var (ref None)
      | [t] -> Var (ref (Some (of_sexpr t)))
      | _ -> invalid_arg "unexpected Type.t")
  | _ -> invalid_arg "unexpected Type.t" 


let rec to_sexpr = function
  | Unit -> Sexpr.Sident "t:unit"
  | Bool -> Sexpr.Sident "t:bool"
  | Int -> Sexpr.Sident "t:int"
  | Float -> Sexpr.Sident "t:float"
  | Char -> Sexpr.Sident "t:char"
  | Fun (t1s, t2) ->
    Sexpr.Sexpr [Sexpr.Sident "t:fun"; Sexpr.Sexpr (List.map to_sexpr t1s); to_sexpr t2]
  | Tuple ts -> 
    Sexpr.Sexpr (Sexpr.Sident "t:tuple" :: (List.map to_sexpr ts))
  | List t -> 
    Sexpr.Sexpr [Sexpr.Sident "t:list";  to_sexpr t]
  | Array t -> 
    Sexpr.Sexpr [Sexpr.Sident "t:array";  to_sexpr t]
  | Variant t -> 
    Sexpr.Sexpr [Sexpr.Sident "t:variant";  Sexpr.Sident t]
  | Var x -> 
    Sexpr.Sexpr (Sexpr.Sident "t:var" :: match !x with
      | None -> []
      | Some i -> [to_sexpr i])

let rec oType_to_sexpr = function
  | O_Constant t -> Sexpr.Sexpr [Sexpr.Sident "ot:constant"; to_sexpr t]
  | O_Variable t -> Sexpr.Sexpr [Sexpr.Sident "ot:var"; Sexpr.Sident t]
  | O_Tuple ts -> Sexpr.Sexpr (Sexpr.Sident "ot:tuple" :: (List.map oType_to_sexpr ts))
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

let rec oType_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "ot:constant"; t] -> O_Constant (of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "ot:var"; Sexpr.Sident t] -> O_Variable t
  | Sexpr.Sexpr (Sexpr.Sident "ot:tuple" :: ts) -> O_Tuple (List.map oType_of_sexpr ts)
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
  | Substitution(tv, ot) -> Sexpr.Sexpr[Sexpr.Sident tv; oType_to_sexpr ot]

let rec substitution_of_sexpr = function
  | Sexpr.Sexpr[Sexpr.Sident tv; ot] -> Substitution (tv, oType_of_sexpr ot)
  | _ -> invalid_arg "unexpected token."

let rec substitutions_to_sexpr ss =
  Sexpr.Sexpr ((Sexpr.Sident "substs") :: List.map substitution_to_sexpr ss)
 
let rec substitutions_of_sexpr = function
  | Sexpr.Sexpr ((Sexpr.Sident "substs") :: ss) -> 
    List.map substitution_of_sexpr ss
  | _ -> invalid_arg "unexpected token."
