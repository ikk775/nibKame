type typeVar = Id.t
type exprVar = Id.t
type exprConst = Id.t
type expr =
    E_Constant of exprConst
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Let of exprVar * expr * expr
type oType = O_Constant | O_Variable of typeVar | O_Fun of oType * oType
type typeScheme = OType of oType | QType of typeVar list * typeScheme
type typeEnv = TypeEnv of (exprVar * typeScheme) list
type subst = Subst of typeVar * oType
val typeVars : oType -> typeVar list
val freeTypeVars : typeScheme -> typeVar list
val freeTypeVarsEnv : typeEnv -> typeVar list
