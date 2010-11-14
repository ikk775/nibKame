type exprVar = Id.t
type exprConst = Id.t
type expr =
    E_Constant of Syntax.lit
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Tuple of expr list
  | E_If of expr * expr * expr
  | E_Let of exprVar * expr * expr
  | E_Fix of exprVar * expr
  | E_Type of expr * Type.oType
type exprEnv = ExprEnv of (Id.t * Type.oType) list
val extExprEnv : (string * Type.oType) list ref
val getExtExprEnv : (string * Type.oType) list
val setExtExprEnv : (string * Type.oType) list -> unit
val addExtExprEnv : string * Type.oType -> unit
exception ExtFun_not_found of string
val getConstantType : expr -> Type.oType
val getVariableType : Type.typeEnv -> expr -> Type.typeScheme
val substituteExpr : Type.substitution list -> expr -> expr
val w : Type.typeEnv -> expr -> Type.substitution list * Type.oType * expr
