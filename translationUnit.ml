open MyUtil

type t = Syntax.t list

let read_sexprs : char Stream.t -> Sexpr.t list = fun stm -> 
  let f stm =
    try
      Some (Sexpr.read stm)
    with
      | Stream.Failure -> None
  in
  let rec g es =
    match f stm with
      | None -> es 
      | Some e -> g (e :: es)
  in
  List.rev (g [])

let add_def teenv m = function
  | Syntax.TopLet (pat, e) ->
    undefined ()
  | Syntax.TopLetSimp ((x, t), e) ->
    Module.add_expr_with_env teenv m (x, (TypingExpr.from_syntax e))
  | Syntax.TopLetRec ((x, t), e) ->
    Module.add_expr_with_env teenv m (x, (TypingExpr.E_Fix (x, (TypingExpr.from_syntax e))))
  | _ -> invalid_arg "unexpected keyword"

let read : char Stream.t -> Syntax.t list = fun stm -> 
  List.map Sread.change (read_sexprs stm)

let modulize : TypingExpr.exprEnv -> Syntax.t list -> Module.t = fun teenv es -> 
  List.fold_left (add_def teenv) Module.empty es
