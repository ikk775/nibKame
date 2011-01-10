type exprVar = Id.t
type exprConst = Id.t
type expr =
  | E_Constant of Syntax.lit
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Tuple of expr list
  | E_Vector of expr list
  | E_If of expr * expr * expr
  | E_Let of exprVar * expr * expr
  | E_Fix of exprVar * expr
  | E_Match of expr * (pattern * expr * expr) list
  | E_External of exprVar * TypingType.oType
  | E_Type of expr * TypingType.oType
  | E_Declare of exprVar * TypingType.oType * expr
and pattern =
  | EP_Constant of Syntax.lit
  | EP_Variable of Id.t option
  | EP_Constructor of Id.t
  | EP_Apply of pattern * pattern
  | EP_And of pattern * pattern
  | EP_Or of pattern * pattern (* Both patterns must have a same set of variables. And each variable has same type across the patterns. *)
  | EP_Not of pattern
  | EP_Tuple of pattern list
  | EP_Vector of pattern list
type t = expr
type exprEnv = ExprEnv of (Id.t * TypingType.typeScheme) list
type substitution = exprVar * expr
val empty_exprEnv : exprEnv
val extexprenv : (string * TypingType.oType) list ref
val get_extexprenv : (string * TypingType.oType) list
val set_extexprenv : (string * TypingType.oType) list -> unit
val add_extexprenv : string * TypingType.oType -> unit
val gen_exprvar_num : int ref
val gen_exprvar : unit -> expr
val gen_exprvars : int -> expr list
val substitute_env : TypingType.substitution list -> exprEnv -> exprEnv
val add_env : exprEnv -> Id.t -> TypingType.typeScheme -> exprEnv
val combine_env : exprEnv -> exprEnv -> exprEnv
val freetypevars_env : exprEnv -> TypingType.typeVar list
val clos : exprEnv -> TypingType.typeScheme -> TypingType.typeScheme
exception ExtFun_not_found of string
val get_constant_type : expr -> TypingType.oType
exception Variable_not_found of string
val get_variable_type : exprEnv -> expr -> TypingType.typeScheme
val get_exprvar_name : expr -> exprVar
val get_expr_type : expr -> TypingType.oType
val substitution_domain : substitution list -> exprVar list
val reverse_substitution : substitution list -> substitution list
val substitute_expr : (exprVar * expr) list -> expr -> expr
val substitute_expr_type : TypingType.substitution list -> expr -> expr
val compose_expr_subst : substitution list -> substitution list -> substitution list
val from_syntax : Syntax.t -> expr
val to_sexpr : expr -> Sexpr.t
val of_sexpr : Sexpr.t -> expr
val pattern_to_sexpr : pattern -> Sexpr.t
val pattern_of_sexpr : Sexpr.t -> pattern
val substitution_to_sexpr : exprVar * expr -> Sexpr.t
val substitution_of_sexpr : Sexpr.t -> exprVar * expr
val substitutions_to_sexpr : (exprVar * expr) list -> Sexpr.t
val substitutions_of_sexpr : Sexpr.t -> (exprVar * expr) list
