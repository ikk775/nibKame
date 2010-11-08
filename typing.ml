


type typeVar = Id.t
type exprVar = Id.t
type exprConst = Id.t

type expr =
  | E_Constant of exprConst
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Let of exprVar * expr * expr

type oType =
  | O_Constant
  | O_Variable of typeVar
  | O_Fun of oType * oType

type typeScheme =
  | OType of oType
  | QType of typeVar list * typeScheme

type typeEnv =
  | TypeEnv of (exprVar * typeScheme) list

type subst =
  | Subst of typeVar * oType

let rec typeVars  = function
  | O_Constant -> []
  | O_Variable tv -> [tv]
  | O_Fun(t1, t2) -> ExtList.List.unique(List.append (typeVars t1) (typeVars t2))

let rec freeTypeVars = function
  | OType t -> typeVars t
  | QType(qv, ts) -> List.filter (fun x -> not (List.mem x qv)) (freeTypeVars ts)

let rec freeTypeVarsEnv = function
  | TypeEnv l -> ExtList.List.unique (List.fold_left (fun a b -> List.append a (freeTypeVars (snd b))) [] l)
  
  
let () = print_string (Std.dump (MyUtil.String.explode "abc"))
let () = print_string (Std.dump (MyUtil.String.explode "abc"))