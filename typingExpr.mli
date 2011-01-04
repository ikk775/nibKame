type exprVar = Id.t
type exprConst = Id.t
type expr =
    E_Constant of Syntax.lit
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Tuple of expr list
  | E_Vector of expr list
  | E_If of expr * expr * expr
  | E_Let of exprVar * expr * expr
  | E_Fix of exprVar * expr
  | E_External of exprVar * TypingType.oType
  | E_Type of expr * TypingType.oType
  | E_Declare of exprVar * TypingType.oType * expr
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
val get_variable_type : exprEnv -> expr -> TypingType.typeScheme
val get_exprvar_name : expr -> exprVar
val get_expr_type : expr -> TypingType.oType
val substitute_expr : (exprVar * expr) list -> expr -> expr
val substitute_expr_type : TypingType.substitution list -> expr -> expr
val from_syntax : Syntax.t -> expr
val to_sexpr : expr -> Sexpr.t
val of_sexpr : Sexpr.t -> expr
val substitution_to_sexpr : exprVar * expr -> Sexpr.t
val substitution_of_sexpr : Sexpr.t -> exprVar * expr
val substitutions_to_sexpr : (exprVar * expr) list -> Sexpr.t
val substitutions_of_sexpr : Sexpr.t -> (exprVar * expr) list
