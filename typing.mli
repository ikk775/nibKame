type typeVar = Id.t
type typeConst = Id.t
type exprVar = Id.t
type exprConst = Id.t
type expr =
    E_Constant of exprConst
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Let of exprVar * expr * expr
type oType =
    O_Constant of typeConst
  | O_Variable of typeVar
  | O_Fun of oType * oType
type typeScheme = OType of oType | QType of typeVar list * typeScheme
type typeEnv = TypeEnv of (exprVar * typeScheme) list
type substitution = Substitution of typeVar * oType
val genTypeVarNum : int ref
val genTypeVar : unit -> oType
val genTypeVars : int -> oType list
val typeVars : oType -> typeVar list
val freeTypeVars : typeScheme -> typeVar list
val freeTypeVarsEnv : typeEnv -> typeVar list
val targetVars : substitution list -> oType list
val substitute : substitution list -> oType -> oType
val composite : substitution list -> substitution list -> substitution list
val supp : substitution list -> oType list
val eq_subst : substitution list -> substitution list -> bool
val substituteTs : substitution list -> typeScheme -> typeScheme
val substituteEnv :
  substitution list -> ('a * typeScheme) list -> ('a * typeScheme) list
val addEnv : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
val clos : typeEnv -> typeScheme -> typeScheme
