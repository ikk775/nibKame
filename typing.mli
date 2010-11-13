type typeVar = Id.t
type typeConst = Id.t
type exprVar = Id.t
type exprConst = Id.t
type oType =
    O_Constant of typeConst
  | O_Variable of typeVar
  | O_Fun of oType * oType
type typeScheme = OType of oType | QType of typeVar list * typeScheme
type typeEnv = TypeEnv of (exprVar * typeScheme) list
type expr =
    E_Constant of exprConst
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Let of exprVar * expr * expr
  | E_Fix of exprVar * expr
  | E_Type of expr * oType
type substitution = Substitution of typeVar * oType
val getConstantType : expr -> oType
val getVariableType : typeEnv -> expr -> typeScheme
val genTypeVarNum : int ref
val genTypeVar : unit -> oType
val genTypeVars : int -> oType list
val removeQuantifier : typeScheme -> oType
val typeVars : oType -> typeVar list
val occur : typeVar -> oType -> bool
val freeTypeVars : typeScheme -> typeVar list
val freeTypeVarsEnv : typeEnv -> typeVar list
val targetVars : substitution list -> oType list
val substitute : substitution list -> oType -> oType
val composite : substitution list -> substitution list -> substitution list
val supp : substitution list -> oType list
val eq_subst : substitution list -> substitution list -> bool
val substituteTs : substitution list -> typeScheme -> typeScheme
val substituteEnv : substitution list -> typeEnv -> typeEnv
val substituteEqnPair : substitution list -> oType * oType -> oType * oType
val substituteEqnPairs :
  substitution list -> (oType * oType) list -> (oType * oType) list
val addEnv : typeEnv -> exprVar -> typeScheme -> typeEnv
val clos : typeEnv -> typeScheme -> typeScheme
exception Unification_Failure of (oType * oType) list * substitution list
val unify_u : (oType * oType) list -> substitution list -> substitution list
val unify : oType -> oType -> substitution list
val w : typeEnv -> expr -> substitution list * oType * expr
