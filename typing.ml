type typeVar = Id.t
type typeConst = Id.t
type exprVar = Id.t
type exprConst = Id.t

type expr =
  | E_Constant of exprConst
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Let of exprVar * expr * expr
  | E_Fix of exprVar * expr

type oType =
  | O_Constant of typeConst
  | O_Variable of typeVar
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
  O_Variable ("$" ^ string_of_int !genTypeVarNum)

let rec genTypeVars n =
  genTypeVar () :: genTypeVars (n - 1)

let rec typeVars  = function
  | O_Constant _ -> []
  | O_Variable tv -> [tv]
  | O_Fun(t1, t2) -> ExtList.List.unique(List.append (typeVars t1) (typeVars t2))

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
  List.map (function ftw, tts -> ftw, substituteTs ss tts) env
  
let addEnv env tv ts =
  (tv, ts) :: List.remove_assoc tv env
  
let clos env ts =
  let freeVars = MyUtil.List.setDiff (freeTypeVars ts) (freeTypeVarsEnv env) in
  QType(freeVars, ts)