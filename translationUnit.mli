type t = Syntax.t list
val read_sexprs : char Stream.t -> Sexpr.t list
val read : char Stream.t -> Syntax.t list
val modulize : TypingExpr.exprEnv -> Syntax.t list -> Module.t
