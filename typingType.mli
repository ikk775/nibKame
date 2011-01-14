type typeVar = Id.t
type typeConst = Id.t
type exprVar = Id.t
module TypeVarSet :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
type oType =
    O_Constant of Type.t
  | O_Variable of typeVar
  | O_Tuple of oType list
  | O_Vector of oType
  | O_Variant of oType * oType
  | O_Fun of oType * oType
  | O_Ref of oType
type typeScheme = OType of oType | QType of typeVar list * typeScheme
type t = typeScheme
type typeEnv = TypeEnv of (exprVar * typeScheme) list
type substitution = Substitution of typeVar * oType
val arg_type : oType -> oType
val dest_type : oType -> oType
val gen_typevar_num : int ref
val gen_typevar : unit -> oType
val gen_typevars : int -> oType list
val get_oType_variable_name : oType -> typeVar
val remove_quantifier : typeScheme -> oType
val bindedVars : typeScheme -> typeVar list
val typevars : oType -> TypeVarSet.elt list
val occur : TypeVarSet.elt -> oType -> bool
val freetypevars : typeScheme -> typeVar list
val normalize_typeScheme : typeScheme -> typeScheme
val freetypevars_env : typeEnv -> TypeVarSet.elt list
val domain : substitution list -> typeVar list
val targetVars : substitution list -> oType list
val domain_restrict : substitution list -> typeVar list -> substitution list
val domain_diff : substitution list -> typeVar list -> substitution list
val substitute : substitution list -> oType -> oType
val compose : substitution list -> substitution list -> substitution list
val compose_substs : substitution list list -> substitution list
val supp : substitution list -> oType list
val eq_subst : substitution list -> substitution list -> bool
val substitute_ts : substitution list -> typeScheme -> typeScheme
val substitute_env : substitution list -> typeEnv -> typeEnv
val add_env : typeEnv -> exprVar -> typeScheme -> typeEnv
val clos : typeEnv -> typeScheme -> typeScheme
val substitute_eqnpair : substitution list -> oType * oType -> oType * oType
val substitute_eqnpairs :
  substitution list -> (oType * oType) list -> (oType * oType) list
exception Unification_Failure of (oType * oType) list * substitution list
val unify_u : (oType * oType) list -> substitution list -> substitution list
val unify : oType -> oType -> substitution list
val renew : oType -> oType -> substitution list
val oType_of_type : Type.t -> oType
val oType_to_type : oType -> Type.t
val oType_to_mType : oType -> Type.mType
val oType_to_sexpr : oType -> Sexpr.t
val oType_of_sexpr : Sexpr.t -> oType
val typeScheme_to_sexpr : typeScheme -> Sexpr.t
val typeScheme_of_sexpr : Sexpr.t -> typeScheme
val typeEnv_to_sexpr : typeEnv -> Sexpr.t
val typeEnv_of_sexpr : Sexpr.t -> (string * typeScheme) list
val substitution_to_sexpr : substitution -> Sexpr.t
val substitution_of_sexpr : Sexpr.t -> substitution
val substitutions_to_sexpr : substitution list -> Sexpr.t
val substitutions_of_sexpr : Sexpr.t -> substitution list
