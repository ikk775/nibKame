type elt =
  | Type of Id.t * (Id.t list * TypingType.oType)
  | Expr of Id.t * (Id.t list * TypingType.typeScheme * Typing.result)
(*  | Generic of Id.t * (TypingType.typeScheme * (Id.t * TypingType.oType list) list) *)

type substitutions = {
  s_Type: TypingType.substitution list;
  s_Expr: Typing.substitution list;
  s_Expr_TE: TypingExpr.substitution list;
  }

type extToIntMap = {
  eim_Type: TypingType.substitution list;
  eim_Expr: TypingExpr.substitution list;
  }

type intToExtMap = Id.substitution list

type t = {
  eim: extToIntMap;
  iem: intToExtMap;
  defs: elt list
}
val empty : t
val emptyeim : extToIntMap
val emptysubst : substitutions
val elt_name : elt -> Id.t
val defs : t -> elt list
val defs_type : t -> elt list
val defs_expr : t -> elt list
val defs_type_cont : t -> (Id.t * (Id.t list * TypingType.oType)) list
val defs_expr_cont :
  t -> (Id.t * (Id.t list * TypingType.typeScheme * Typing.result)) list
val def_exprTypes : t -> (Id.t * TypingType.typeScheme) list
val def_expr :
  t -> Id.t -> Id.t * (Id.t list * TypingType.typeScheme * Typing.result)
val def_type : t -> Id.t -> Id.t * (Id.t list * TypingType.oType)
val add_def_tail : t -> elt -> t
val add_def_head : t -> elt -> t
val expr_env : t -> TypingExpr.exprEnv
val ext_expr_env : t -> TypingExpr.exprEnv
val subst : substitutions -> t -> t
val add_type :
  t -> TypingType.typeVar * (TypingType.typeVar list * TypingType.oType) -> t
val add_expr_with_env : TypingExpr.exprEnv -> t -> TypingExpr.exprVar * TypingExpr.expr -> t
val add_expr : t -> TypingExpr.exprVar * TypingExpr.expr -> t
val add_expr_instance :
  t -> Typing.resultVar * TypingType.oType * Typing.result -> t
val freeexprvars : t -> (Typing.resultVar * TypingType.oType) list
val freetypevars : t -> TypingType.typeVar list
val coerce_freetypevars : TypingType.oType -> t -> t
val coerce_typevars : TypingType.oType -> t -> t
val unused_exprvars : t -> Id.t list
val remove_expr : t -> Id.t list -> t
val elt_to_sexpr : elt -> Sexpr.t
val gather_expr : t -> Typing.result
val to_sexpr : t -> Sexpr.t
val compose : t -> t -> t
