type resultVar = Id.t
type result =
    R_Constant of Syntax.lit * TypingType.oType
  | R_Variable of resultVar * TypingType.oType
  | R_Fun of (resultVar * TypingType.oType) * result
  | R_Apply of result * result
  | R_Tuple of result list * TypingType.oType
  | R_Vector of result list * TypingType.oType
  | R_If of result * result * result
  | R_Let of (resultVar * TypingType.oType) * result * result
  | R_Fix of (resultVar * TypingType.oType) * result * TypingType.oType
  | R_Match of result * (pattern * result * result) list
  | R_External of Id.t * TypingType.oType
and pattern =
  | RP_Constant of Syntax.lit * TypingType.oType
  | RP_Variable of Id.t option * TypingType.oType
  | RP_Constructor of Id.t * TypingType.oType
  | RP_Apply of (pattern * pattern) * TypingType.oType
  | RP_And of (pattern * pattern) * TypingType.oType
  | RP_Or of (pattern * pattern) * TypingType.oType (* Both patterns must have a same set of variables. And each variable has same type across the patterns. *)
  | RP_Not of pattern * TypingType.oType
  | RP_Tuple of pattern list * TypingType.oType
  | RP_Vector of pattern list * TypingType.oType
val bindedvars : result -> (Id.t * TypingType.oType) list
val freevars : result -> (resultVar * TypingType.oType) list
type substitution = (resultVar * TypingType.oType) * result
val typevars : result -> Id.t list
val gen_varname : unit -> string
val gen_var : TypingType.oType -> result
val gen_varnames : int -> string list
val varname : result -> resultVar
val result_freetypevars : (Id.t * TypingType.oType) list -> Id.t list
val value_restrict : result -> TypingType.oType -> TypingType.typeScheme
val substitute_result_type : TypingType.substitution list -> result -> result
val result_to_expr : result -> TypingExpr.expr
val w : TypingExpr.exprEnv -> TypingExpr.expr -> TypingType.substitution list * TypingType.oType * result
val w_pattern : TypingExpr.exprEnv -> TypingExpr.pattern -> TypingType.substitution list * TypingType.oType * pattern
val typing_with_subst : TypingExpr.exprEnv -> TypingExpr.expr -> TypingType.typeScheme * result * TypingType.substitution list
val typing : TypingExpr.exprEnv -> TypingExpr.expr -> TypingType.typeScheme * result
val substitute : substitution list -> result -> result
val substitute_with_expr_subst : (TypingExpr.exprVar * TypingExpr.expr) list -> result -> result
val of_sexpr : Sexpr.t -> result
val gather : resultVar * TypingType.oType * result -> result -> result
val to_sexpr : result -> Sexpr.t
val result_type : result -> TypingType.oType
