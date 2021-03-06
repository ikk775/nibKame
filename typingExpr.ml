open MyUtil
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
  | E_Match of expr * (pattern * expr option * expr) list 
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

type exprEnv =
  | ExprEnv of (Id.t * TypingType.typeScheme) list

type substitution = exprVar * expr

let extexprenv = ref []

let empty_exprEnv = ExprEnv []

let get_extexprenv = !extexprenv
let set_extexprenv eenv =
  extexprenv := eenv
let add_extexprenv tp =
  extexprenv := tp :: !extexprenv
 
let gen_exprvar_num = ref 0

let gen_exprvar stem =
  gen_exprvar_num := !gen_exprvar_num + 1;
  E_Variable (Format.sprintf "%s_IE%d" (Mangle.escapex "_NE__" stem) !gen_exprvar_num)

let rec gen_exprvars = function
  | [] -> []
  | s::xs -> gen_exprvar s :: gen_exprvars xs

let substitute_env ss env =
  match env with
    | ExprEnv envList -> ExprEnv (List.map (function ftw, tts -> ftw, TypingType.substitute_ts ss tts) envList)

let add_env = function ExprEnv envList -> fun ev ts -> 
  ExprEnv((ev, ts) :: List.remove_assoc ev envList)

let combine_env env = function ExprEnv envlist ->
  List.fold_left (fun env -> function ev, ts -> add_env env ev ts) env envlist
  

let rec freetypevars_env = function
  | ExprEnv l ->
    List.unique (List.concat (List.map (fun x -> TypingType.freetypevars (snd x)) l))

let clos env ts =
  let freeVars = MyUtil.List.setDiff (TypingType.freetypevars ts) (freetypevars_env env) in
  TypingType.QType(freeVars, ts)

let rec pattern_subst f_leaf pat =
  let rec g pat = match pat with
    | EP_Constant _ | EP_Variable _ | EP_Constructor _ -> f_leaf pat
    | EP_Apply (p1, p2) -> EP_Apply (g p1, g p2)
    | EP_And (p1, p2) -> EP_And (g p1, g p2)
    | EP_Or (p1, p2) -> EP_Or (g p1, g p2)
    | EP_Not p -> EP_Not (g p)
    | EP_Tuple ps -> EP_Tuple (List.map g ps)
    | EP_Vector ps -> EP_Vector (List.map g ps)
  in
  g pat

let rec walk_pattern_leaf f_leaf pat =
  let rec g pat = match pat with
    | EP_Constant _ | EP_Variable _ | EP_Constructor _ -> f_leaf pat
    | EP_Apply (p1, p2) | EP_And (p1, p2) | EP_Or (p1, p2) -> g p1 @ g p2
    | EP_Not p -> g p
    | EP_Tuple ps | EP_Vector ps -> List.concat (List.map g ps)
  in
  g pat

let pattern_freevars pat =
  let f = function
    | EP_Constant _  -> []
    | EP_Variable None -> [] 
    | EP_Variable (Some v) -> [v] 
    | EP_Constructor _ -> []
    | EP_Apply _ | EP_And _ | EP_Or _ | EP_Not _ | EP_Tuple _ | EP_Vector _ -> failwith "something went wrong."
  in
  walk_pattern_leaf f pat

let pattern_constructors pat =
  let f = function
    | EP_Constant _  -> []
    | EP_Variable _ -> [] 
    | EP_Constructor v -> [v]
    | EP_Apply _ | EP_And _ | EP_Or _ | EP_Not _ | EP_Tuple _ | EP_Vector _ -> failwith "something went wrong."
  in
  walk_pattern_leaf f pat

exception ExtFun_not_found of string
let get_constant_type = function
  | E_Constant c ->
    (match c with
      | Syntax.Unit -> TypingType.O_Constant Type.Unit
      | Syntax.Nil -> TypingType.O_Variant (TypingType.gen_typevar "_AE__Nil", TypingType.O_Constant (Type.Variant "list"))
      | Syntax.Bool _ -> TypingType.O_Constant Type.Bool
      | Syntax.Int _ -> TypingType.O_Constant Type.Int
      | Syntax.Float _ -> TypingType.O_Constant Type.Float
      | Syntax.Char _ -> TypingType.O_Constant Type.Char
      | Syntax.ExtFun f -> 
        (try
          List.assoc f !extexprenv
        with
          | Not_found -> raise (ExtFun_not_found f)))
  | _ -> invalid_arg "expected type E_Constant"

exception Variable_not_found of string
let get_variable_type env expr =
  match env, expr with
    | ExprEnv envList, E_Variable v ->
      begin try
        List.assoc v envList
      with
        | Not_found -> raise (Variable_not_found v)
      end
    | _ -> invalid_arg "expected type E_Variable"

let get_exprvar_name = function
  | E_Variable v -> v
  | _ -> invalid_arg "expected type E_Variable"

let rec get_expr_type = function
  | E_Type (_, t) -> t
  | E_Let (_, _, e) -> get_expr_type e
  | E_If (_, _, e) -> get_expr_type e
  | E_Declare (_, _, e) -> get_expr_type e
  | E_Fun (_, e) -> get_expr_type e
  | _ -> invalid_arg "expected type-infered expr"

let substitution_domain ss =
  List.map fst ss

let reverse_substitution ss =
  let f = function
    | v, E_Variable v' -> v', E_Variable v
    | _ -> failwith "Only inter-variable substitution can be reversed."
  in
  List.map f ss

let rec substitute_expr ss expr =
  let subst = substitute_expr ss in
  match expr with
    | E_Variable v ->
      if List.mem_assoc v ss 
      then List.assoc v ss
      else E_Variable v
    | E_Constant c -> E_Constant c
    | E_Fun(v, e) ->
      let ss' = List.remove_assoc v ss in
      let subst' = substitute_expr ss' in
      E_Fun(v, subst' e)
    | E_Apply(e1, e2) -> E_Apply(subst e1, subst e2)
    | E_Tuple(es) -> E_Tuple(List.map subst es)
    | E_Vector(es) -> E_Vector(List.map subst es)
    | E_If(e1, e2, e3) -> E_If(subst e1, subst e2, subst e3)
    | E_Let(v, e1, e2) ->
      let ss' = List.remove_assoc v ss in
      let subst' = substitute_expr ss' in
      E_Let(v, subst e1, subst' e2)
    | E_Fix(f, e) ->
      let ss' = List.remove_assoc f ss in
      let subst' = substitute_expr ss' in
      E_Fix(f, subst' e)
    | E_External(s, t) -> E_External(s, t)
    | E_Match(e, cls) ->
      let rec f p = match p with
        | EP_Constant _ | EP_Variable _ | EP_Constructor _ -> p
        | EP_Apply (p1, p2) -> EP_Apply (f p1, f p2)
        | EP_And (p1, p2) -> EP_And (f p1, f p2)
        | EP_Or (p1, p2) -> EP_Or (f p1, f p2)
        | EP_Not p -> EP_Not (f p)
        | EP_Tuple ps -> EP_Tuple (List.map f ps)
        | EP_Vector ps -> EP_Vector (List.map f ps)
      in
      let g = function
        | p, Some guard, e -> f p, Some (subst guard), subst e
        | p, None, e -> f p, None, subst e in
      E_Match (subst e, List.map g cls)
    | E_Type(e, t) -> E_Type(subst e, t)
    | E_Declare(v, t, e) ->
      let v' = gen_exprvar (v) in
      let v'c = get_exprvar_name v' in
      if List.mem_assoc v ss 
      then E_Let(v'c, List.assoc v ss, E_Declare(v'c, t, subst e))
      else E_Declare(v, t, subst e)

let rec substitute_varname ss expr =
  substitute_expr (List.map (function v, v' -> v, E_Variable v') ss) expr

let rec substitute_expr_type ss expr =
  let subst = substitute_expr_type ss in
  match expr with
    | E_Variable v -> E_Variable v
    | E_Constant c -> E_Constant c
    | E_Fun(v, e) -> E_Fun(v, subst e)
    | E_Apply(e1, e2) -> E_Apply(subst e1, subst e2)
    | E_Tuple(es) -> E_Tuple(List.map subst es)
    | E_Vector(es) -> E_Vector(List.map subst es)
    | E_If(e1, e2, e3) -> E_If(subst e1, subst e2, subst e3)
    | E_Let(v, e1, e2) -> E_Let(v, subst e1, subst e2)
    | E_Fix(f, e) -> E_Fix(f, subst e)
    | E_External(s, t) -> E_External(s, TypingType.substitute ss t)
    | E_Match(e, cls) ->
      let rec f p = match p with
        | EP_Constant _ | EP_Variable _ | EP_Constructor _ -> p
        | EP_Apply (p1, p2) -> EP_Apply (f p1, f p2)
        | EP_And (p1, p2) -> EP_And (f p1, f p2)
        | EP_Or (p1, p2) -> EP_Or (f p1, f p2)
        | EP_Not p -> EP_Not (f p)
        | EP_Tuple ps -> EP_Tuple (List.map f ps)
        | EP_Vector ps -> EP_Vector (List.map f ps)
      in
      let g = function
        | p, Some guard, e -> f p, Some (subst guard), subst e
        | p, None, e -> f p, None, subst e in
      E_Match (subst e, List.map g cls)
    | E_Type(e, t) -> E_Type(subst e, TypingType.substitute ss t)
    | E_Declare(v, t, e) -> E_Declare(v, TypingType.substitute ss t, subst e)

let rec compose_expr_subst xs ys =
  let subst_v x ys = (List.map (fun y -> match y with fv, te -> (fv ,substitute_expr [x] te)) ys) in
  match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, ys ->
      if (List.exists (fun y -> 
        match x, y with
          | (fvx, _), (fvy, _) -> fvx = fvy) ys)
      then compose_expr_subst xs (subst_v x ys)
      else compose_expr_subst xs (x :: subst_v x ys)

(*i
type pat =
  | P_Ident of Id.t
  | P_Literal of lit
  | P_Tuple of pat list
  | P_List of pat list
  | P_Array of pat list
  | P_Variant of Id.t * pat list
  | Any
i*)

let rec from_syntax = function
  | Syntax.Literal l -> E_Constant l
  | Syntax.LetSimp ((v, t), e1, e2) -> E_Let(v, from_syntax e1, from_syntax e2)
  | Syntax.If (e1, e2, e3) -> E_If(from_syntax e1, from_syntax e2, from_syntax e3)
  | Syntax.Fun (vts, e) -> 
    let f acc v = E_Fun(v, acc) in
    let vs = fst (List.split vts) in
    List.fold_left f (from_syntax e) (List.rev vs)
  | Syntax.Var v -> E_Variable v
  | Syntax.Apply (f, es) -> 
    let g acc e = E_Apply(acc, from_syntax e) in
    List.fold_left g (from_syntax f) es
  | Syntax.Tuple es -> 
    E_Tuple(List.map from_syntax es)
  | Syntax.List es -> 
    let f e acc= E_Apply(E_Apply(E_Variable "cons", from_syntax e), acc) in
    List.fold_right f es (E_Constant Syntax.Nil)
  | Syntax.Array es -> 
    E_Apply(E_Apply(E_Variable "array-from-list-with-length", E_Constant (Syntax.Int (List.length es))), from_syntax (Syntax.List es))
  | Syntax.Fix ((v, t), e) -> E_Fix(v, from_syntax e)
  | Syntax.Match (e, cls) -> 
    let f = function
      | pat, Some guard, expr ->
        pattern_from_syntax_pattern pat, Some (from_syntax guard), from_syntax expr
      | pat, None, expr ->
        pattern_from_syntax_pattern pat, None, from_syntax expr
    in
    E_Match (from_syntax e, List.map f cls)
  | Syntax.Let (Syntax.P_Ident v, e1, e2) -> E_Let (v, from_syntax e1, from_syntax e2)
  | Syntax.Let (pat, e1, e2) -> E_Match (from_syntax e1, [pattern_from_syntax_pattern pat, None, from_syntax e2])
  | Syntax.Variant v -> E_Variable v
  | Syntax.LetRec ((v, t), e1, e2) ->
    let b = gen_exprvar (v) in
    let bn = get_exprvar_name b in
    let ss = [v, b] in
    E_Let(v, E_Fix (bn, substitute_expr ss (from_syntax e1)), from_syntax e2)
  | Syntax.TopLet _ | Syntax.TopLetRec _ | Syntax.TopLetSimp _ -> invalid_arg "from_syntax"
and pattern_from_syntax_pattern = function
  | Syntax.P_Ident v -> EP_Variable (Some v)
  | Syntax.P_Literal lit -> EP_Constant lit 
  | Syntax.P_Tuple ps -> EP_Tuple (List.map pattern_from_syntax_pattern ps)
  | Syntax.P_List ps -> (undefined ())
  | Syntax.P_Array ps -> (undefined ())
  | Syntax.P_Variant (v, ps) ->
    let ps' = List.rev ps in
    let rec f = function
      | [] -> EP_Constructor v
      | p :: ps -> EP_Apply (f ps, pattern_from_syntax_pattern p)
     in
    f ps'
  | Syntax.P_And _ | Syntax.P_Or _ | Syntax.P_Not _ -> (undefined ())
  | Syntax.Any -> EP_Variable None
let rec to_sexpr = function
  | E_Constant e -> Sexpr.Sexpr [Sexpr.Sident "e:constant"; Syntax.lit_to_sexpr e]
  | E_Variable v -> Sexpr.Sexpr [Sexpr.Sident "e:var"; Sexpr.Sident v]
  | E_Tuple es -> Sexpr.Sexpr (Sexpr.Sident "e:tuple" :: (List.map to_sexpr es))
  | E_Vector es -> Sexpr.Sexpr (Sexpr.Sident "e:vector" :: (List.map to_sexpr es))
  | E_If (e1, e2, e3) -> Sexpr.Sexpr (Sexpr.Sident "e:if" :: (List.map to_sexpr [e1; e2; e3]))
  | E_Let (v, e1, e2) -> Sexpr.Sexpr (Sexpr.Sident "e:let" :: to_sexpr(E_Variable v) :: (List.map to_sexpr [e1; e2]))
  | E_Fix (v, e) -> Sexpr.Sexpr (Sexpr.Sident "e:fix" :: to_sexpr(E_Variable v) :: (List.map to_sexpr [e]))
  | E_Fun(v, e) -> 
    let rec fun_flatten vs = function
      | E_Fun(v, e) ->  fun_flatten (v :: vs) e
      | e -> (List.rev vs), e
    in
    let vs, e' = fun_flatten [] e in
    let vs' = List.map (fun x -> E_Variable x) (v :: vs) in
    Sexpr.Sexpr [Sexpr.Sident "e:fun"; Sexpr.Sexpr(List.map to_sexpr vs'); to_sexpr e']
  | E_Apply(e1, e2) as e-> 
    let rec apply_flatten es = function
      | E_Apply(e1, e2) ->  apply_flatten (e2 :: es) e1
      | e -> e :: es
    in
    Sexpr.Sexpr (Sexpr.Sident "e:apply" ::  List.map to_sexpr (apply_flatten [] e))
  | E_Match(e, cls) ->
    let f = function
      | p, None, e -> Sexpr.Sexpr [pattern_to_sexpr p; to_sexpr e]
      | p, Some g, e -> Sexpr.Sexpr [pattern_to_sexpr p; Sexpr.Sident ":"; to_sexpr g; to_sexpr e]
    in
    Sexpr.tagged_sexpr "e:match" (List.map f cls)
  | E_External(s, t) ->
    Sexpr.Sexpr[Sexpr.Sident "e:extenal"; Sexpr.Sident s; TypingType.oType_to_sexpr t]
  | E_Type(e, t) ->
    Sexpr.Sexpr[Sexpr.Sident "e:type"; to_sexpr e; TypingType.oType_to_sexpr t]
  | E_Declare(v, t, e) ->
    Sexpr.Sexpr[Sexpr.Sident "e:declare"; to_sexpr(E_Variable v); TypingType.oType_to_sexpr t; to_sexpr e]
and pattern_to_sexpr = function
  | EP_Constant lit -> Sexpr.tagged_sexpr "ep:constant" [Syntax.lit_to_sexpr lit]
  | EP_Variable None -> Sexpr.Sident "ep:any"
  | EP_Variable (Some v) -> Sexpr.tagged_sexpr "ep:var" [Sexpr.Sident v]
  | EP_Constructor v -> Sexpr.tagged_sexpr "ep:constructor" [Sexpr.Sident v]
  | EP_Apply (p1, p2) ->
    let rec f ps = function
      | EP_Apply (p1, p2) -> f (p1 :: ps) p2
      | _ as p -> p :: ps
    in
    Sexpr.tagged_sexpr "ep:apply" (pattern_to_sexpr p1 :: List.rev_map pattern_to_sexpr (f [] p2))
  | EP_And (p1, p2) ->
    let rec f ps = function
      | EP_And (p1, p2) -> f (p1 :: ps) p2
      | _ as p -> p :: ps
    in
    Sexpr.tagged_sexpr "ep:and" (pattern_to_sexpr p1 :: List.rev_map pattern_to_sexpr (f [] p2))
  | EP_Or (p1, p2) ->
    let rec f ps = function
      | EP_Or (p1, p2) -> f (p1 :: ps) p2
      | _ as p -> p :: ps
    in
    Sexpr.tagged_sexpr "ep:or" (pattern_to_sexpr p1 :: List.rev_map pattern_to_sexpr (f [] p2))
  | EP_Not p -> Sexpr.tagged_sexpr "ep:not" [pattern_to_sexpr p]
  | EP_Tuple ps -> Sexpr.tagged_sexpr "ep:tuple" (List.map pattern_to_sexpr ps)
  | EP_Vector ps -> Sexpr.tagged_sexpr "ep:vector" (List.map pattern_to_sexpr ps)


let rec of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "e:constant"; e] -> E_Constant (Syntax.lit_of_sexpr e)
  | Sexpr.Sexpr [Sexpr.Sident "e:var"; Sexpr.Sident v] -> E_Variable v
  | Sexpr.Sexpr (Sexpr.Sident "e:tuple" :: es) -> E_Tuple (List.map of_sexpr es)
  | Sexpr.Sexpr (Sexpr.Sident "e:vector" :: es) -> E_Vector (List.map of_sexpr es)
  | Sexpr.Sexpr [Sexpr.Sident "e:if"; e1; e2; e3] -> E_If (of_sexpr e1, of_sexpr e2, of_sexpr e3)
  | Sexpr.Sexpr [Sexpr.Sident "e:let"; v; e1; e2] -> E_Let (get_exprvar_name (of_sexpr v), of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "e:fix"; v; e] -> E_Fix (get_exprvar_name (of_sexpr v), of_sexpr e)
  | Sexpr.Sexpr [Sexpr.Sident "e:fun"; Sexpr.Sexpr vs; e] -> 
    let fun_nest e' vs =
      let rec fun_nest_sub = function
        | [v] ->  E_Fun(get_exprvar_name(of_sexpr v), of_sexpr e')
        | v :: vs ->  E_Fun(get_exprvar_name(of_sexpr v), fun_nest_sub vs)
        | _ -> invalid_arg "unexpected token."
      in
      fun_nest_sub vs
    in
    fun_nest e vs
  | Sexpr.Sexpr (Sexpr.Sident "e:apply" :: e1 :: e2 :: es) -> 
    let rec apply_nest e = function
      | e' :: []->  E_Apply(e, of_sexpr e')
      | e' :: es' -> apply_nest (E_Apply(e, of_sexpr e')) es'
      | _ -> invalid_arg "unexpected token."
    in
    apply_nest (of_sexpr e1) (e2 :: es)
  | Sexpr.Sexpr (Sexpr.Sident "e:match" :: e :: cls) ->
    let f = function
      | Sexpr.Sexpr [p; e] -> pattern_of_sexpr p, None, of_sexpr e
      | Sexpr.Sexpr [p; Sexpr.Sident ":"; g; e] -> pattern_of_sexpr p, Some (of_sexpr g), of_sexpr e
      | _ -> invalid_arg "of_sexpr"
    in
    E_Match (of_sexpr e, List.map f cls)
  | Sexpr.Sexpr [Sexpr.Sident "e:extenal"; Sexpr.Sident s; t] -> E_External(s, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "e:type"; e; t] -> E_Type(of_sexpr e, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "e:declare"; v; t; e] -> E_Declare(get_exprvar_name(of_sexpr v), TypingType.oType_of_sexpr t, of_sexpr e)
  | _ -> invalid_arg "unexpected token."
and pattern_of_sexpr =
  let nest f initial list =
    let rec g p = function
      | [] -> p
      | p' :: ps' -> g (f p p') ps'
    in
    g initial list
  in function
  | Sexpr.Sident "ep:any" -> EP_Variable None
  | Sexpr.Sexpr [Sexpr.Sident "ep:constant"; lit] -> EP_Constant (Syntax.lit_of_sexpr lit)
  | Sexpr.Sexpr [Sexpr.Sident "ep:var"; Sexpr.Sident v] -> EP_Variable (Some v)
  | Sexpr.Sexpr [Sexpr.Sident "ep:constructor"; Sexpr.Sident v] -> EP_Constructor v
  | Sexpr.Sexpr (Sexpr.Sident "ep:apply" :: arg1 :: arg2 :: rest) ->
    let p1 = pattern_of_sexpr arg1 in
    let p2 = pattern_of_sexpr arg2 in
    let ps = List.map pattern_of_sexpr rest in
    nest (fun p p' -> EP_Apply (p, p')) p1 (p2 :: ps)
  | Sexpr.Sexpr (Sexpr.Sident "ep:and" :: arg1 :: arg2 :: rest) ->
    let p1 = pattern_of_sexpr arg1 in
    let p2 = pattern_of_sexpr arg2 in
    let ps = List.map pattern_of_sexpr rest in
    nest (fun p p' -> EP_And (p, p')) p1 (p2 :: ps)
  | Sexpr.Sexpr (Sexpr.Sident "ep:or" :: arg1 :: arg2 :: rest) ->
    let p1 = pattern_of_sexpr arg1 in
    let p2 = pattern_of_sexpr arg2 in
    let ps = List.map pattern_of_sexpr rest in
    nest (fun p p' -> EP_Or (p, p')) p1 (p2 :: ps)
  | Sexpr.Sexpr [Sexpr.Sident "ep:not";  p] ->  EP_Not (pattern_of_sexpr p)
  | Sexpr.Sexpr (Sexpr.Sident "ep:tuple" :: ps) ->  EP_Tuple (List.map pattern_of_sexpr ps)
  | Sexpr.Sexpr (Sexpr.Sident "ep:vector" :: ps) ->  EP_Vector (List.map pattern_of_sexpr ps)
  | _ -> invalid_arg "pattern_of_sexpr"

let rec substitution_to_sexpr = function
  | v, e -> Sexpr.Sexpr (List.map to_sexpr [E_Variable v; e])

let rec substitution_of_sexpr = function
  | Sexpr.Sexpr[v; e] -> (get_exprvar_name (of_sexpr v), of_sexpr e)
  | _ -> invalid_arg "unexpected token."

let rec substitutions_to_sexpr = function
  | ss -> Sexpr.Sexpr (List.map substitution_to_sexpr ss)

let rec substitutions_of_sexpr = function
  | Sexpr.Sexpr (ss) -> 
    List.map substitution_of_sexpr ss
  | _ -> invalid_arg "unexpected token."

let rec exprEnv_to_sexpr = function
  | ExprEnv eqs ->
    let eq_to_sexpr = function
      | v, ts -> Sexpr.Sexpr [Sexpr.Sident v; TypingType.typeScheme_to_sexpr ts]
    in
    Sexpr.Sexpr (Sexpr.Sident "ee:env" :: List.map eq_to_sexpr eqs)

let rec exprEnv_of_sexpr = function
  | Sexpr.Sexpr (Sexpr.Sident "ee:env" :: eqs) -> 
    let eq_of_sexpr = function
      | Sexpr.Sexpr [Sexpr.Sident v; ts] -> v, TypingType.typeScheme_of_sexpr ts
      | _ -> invalid_arg "unexpected token."
    in
    ExprEnv (List.map eq_of_sexpr eqs)
  | _ -> invalid_arg "unexpected token."
    
