open MyUtil

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
  defs: elt list (* the definitions are stored in reverse order. *)
}

let empty:t = {eim = {eim_Type = []; eim_Expr = []}; iem = []; defs = []}

let emptyeim: extToIntMap = {eim_Type = []; eim_Expr = []}

let emptysubst: substitutions = {s_Type = []; s_Expr = []; s_Expr_TE = []}

let compose_eim eim eim' =
  let {eim_Expr = eime; eim_Type = eimt} = eim in
  let {eim_Expr = eime'; eim_Type = eimt'} = eim' in
  {eim_Expr = TypingExpr.compose_expr_subst eime eime'; eim_Type = TypingType.compose eimt eimt'}

let compose_iem iem iem' =
  Id.compose iem iem'

let compose_defs defs defs' =
  defs' @ defs

let elt_name : elt -> Id.t = function
  | Type (x, _)
  | Expr (x, _) -> x

let defs : t -> elt list = fun m ->
  let es = m.defs in
  List.rev es

let defs_type : t -> elt list = fun m -> 
  List.filter (function Type _ -> true | _ -> false) (defs m)

let defs_expr : t -> elt list = fun m -> 
  List.filter (function Expr _ -> true | _ -> false) (defs m)

let exists_type_def m x =
  List.mem x (List.map elt_name (defs_type m))

let exists_expr_def m x =
  List.mem x (List.map elt_name (defs_expr m))

let defs_type_cont = fun m -> 
  List.map (function Type (name, cont) -> name, cont | _ -> failwith "something went wrong!") (defs_type m)

let defs_expr_cont = fun m -> 
  List.map (function Expr (name, cont) -> name, cont | _ -> failwith "something went wrong!") (defs_expr m)

let def_exprTypes = fun m -> 
  List.map (function Expr (x,(qtvs, t, r)) -> x, t | _ -> failwith "something went wrong!") (defs_expr m)

let def_expr = fun m x -> 
  match List.find (function Expr (x', _) when x' = x -> true | _ -> false) (defs_expr m) with
    | Expr (name, cont) -> name, cont
    | _ -> failwith ""
  
let def_type = fun m x -> 
  match List.find (function Type (x', _) when x' = x -> true | _ -> false) (defs_type m) with
    | Type (name, cont) -> name, cont
    | _ -> failwith ""

let add_def_tail : t -> elt -> t = fun m def -> 
  {m with defs = def :: defs m}
 
let add_def_head : t -> elt -> t = fun m def -> 
  {m with defs = List.append (defs m) [def]}

let expr_env : t -> TypingExpr.exprEnv = fun m -> 
	let f = function
    | Expr (x, (qtvs, ts, r)) -> 
		  x, TypingType.QType (qtvs, TypingType.OType (TypingType.remove_quantifier ts))
    | Type _ -> 
	    invalid_arg "Expr requied."
  in
  TypingExpr.ExprEnv (List.map f (defs_expr m))

let ext_expr_env m =
  let TypingExpr.ExprEnv envlist = expr_env m in
  let rss = TypingExpr.reverse_substitution m.eim.eim_Expr in
  let visiblevars = TypingExpr.substitution_domain rss in
  let envlist' = List.filter (function v, c -> List.mem v visiblevars) envlist in
  TypingExpr.ExprEnv (List.map (function v, c -> Id.substitute m.iem v, c) envlist')

let rec subst : substitutions -> t -> t = fun ss m ->
  let typedefs = defs_type m in
  let typedefns = List.map elt_name typedefs in
  let exprdefs = defs_expr m in 
  let exprdefns = List.map elt_name exprdefs in
  match ss with
    | {s_Type = []; s_Expr = []; s_Expr_TE = []} -> m
    | {s_Type = tss; s_Expr = []; s_Expr_TE = []} ->
      let rec substElt = function
        | Type (tv, (qtvs, t)) ->  
          let t' = TypingType.substitute tss t in
          let ftvs = TypingType.freetypevars (TypingType.OType t') in
          let qtvs' = MyUtil.List.setDiff ftvs typedefns in
          Type (tv, (qtvs', t'))
        | Expr (ev, (qtvs, et, e)) -> 
          let e' = Typing.substitute_result_type tss e in
          let oet = TypingType.substitute tss (TypingType.remove_quantifier et) in
          let et' = Typing.value_restrict e' oet in
          let qtvs' = MyUtil.List.setDiff (TypingType.bindedVars et') typedefns in
          let et'' = TypingType.QType (qtvs', TypingType.OType (TypingType.remove_quantifier et')) in
          Expr (ev, (qtvs', et'', e'))
      in
      subst {ss with s_Type = []} {m with defs = List.map substElt m.defs}
    | {s_Type = _; s_Expr = ess; s_Expr_TE = []} ->
      let rec substElt = function
        | Type _ as t-> t
        | Expr (ev, (qtvs, et, r)) -> 
          let r' = Typing.substitute ess r in
          Expr (ev, (qtvs, et, r'))
      in
      subst {ss with s_Expr = []} {m with defs = List.map substElt m.defs}
    | {s_Type = _; s_Expr = []; s_Expr_TE = ess} ->
      let rec substElt = function
        | Type _ as t-> t
        | Expr (ev, (qtvs, et, r)) -> 
          let r' = Typing.substitute_with_expr_subst ess r in
          Expr (ev, (qtvs, et, r'))
      in
      subst {ss with s_Expr_TE = []} {m with defs = List.map substElt m.defs}
    | {s_Type = _; s_Expr = _; s_Expr_TE = _} ->
      failwith "Substitution order is ambiguous."
  
let add_type = fun m -> 
  function
    | tname, (tvs, t) ->
      let eim, iem, es = m.eim, m.iem, m.defs in
      let b = TypingType.gen_typevar () in
      let bn = TypingType.get_oType_variable_name b in
      let tvbs = TypingType.gen_typevars (List.length tvs) in
      let tvbsn = List.map TypingType.get_oType_variable_name tvbs in
      let eim' = {eim with eim_Type =TypingType.compose eim.eim_Type [TypingType.Substitution(tname, b)]} in
      let eim'' = {eim' with eim_Type =TypingType.compose eim.eim_Type (List.map2 (fun f t -> TypingType.Substitution(f, t)) tvs tvbs)} in
      let iem' = Id.compose iem (Id.Substitution(bn, tname) :: List.map2 (fun f t -> Id.Substitution(f, t)) tvbsn tvs) in
      let t' = TypingType.substitute eim''.eim_Type t in
      let substtoname tvn = TypingType.get_oType_variable_name (TypingType.substitute eim''.eim_Type (TypingType.O_Variable tvn)) in
      {m with eim = eim'; iem = iem'; defs = (Type (bn, (List.map substtoname tvs , t')) :: es)}

let rec backpatch m = function
  | [] -> m
  | (fv, ot) :: fvs when exists_expr_def m fv -> 
    Debug.dbgprint (Format.sprintf "backpatching %s" fv);
    let _, (qtvs', t', r') = def_expr m fv in
    let ot' = TypingType.remove_quantifier t' in
    Debug.dbgprintsexpr (TypingType.oType_to_sexpr ot');
    let ss = TypingType.renew ot' ot in
    Debug.dbgprint "subst:";
    Debug.dbgprintsexpr (TypingType.substitutions_to_sexpr ss);
    let ss' = TypingType.domain_diff ss qtvs' in
    Debug.dbgprint "free variables:";
    Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (fun x -> Sexpr.Sident x) qtvs'));
    Debug.dbgprint "free-variable-removed subst:";
    Debug.dbgprintsexpr (TypingType.substitutions_to_sexpr ss');
    let m' = subst {emptysubst with s_Type = ss'} m in
    backpatch m' fvs
  | (fv, ot) :: fvs -> 
    Debug.dbgprint (Format.sprintf "%s is in external module." fv);
    backpatch m fvs

(* top-level let 相当 *)
let add_expr_with_env = fun env m -> 
  function
    | ename, e -> 
      Debug.dbgprint "called add_expr";
      Debug.dbgprint (Format.sprintf "input: %s" ename);
      Debug.dbgprintsexpr (TypingExpr.to_sexpr e);
      let eim, iem, es = m.eim, m.iem, m.defs in
      let e' = TypingExpr.substitute_expr eim.eim_Expr e in
      let t, r = Typing.typing (TypingExpr.combine_env env (expr_env m)) e' in
      Debug.dbgprint "typed input expr";
      Debug.dbgprint "type:";
      Debug.dbgprintsexpr (TypingType.typeScheme_to_sexpr t);
      Debug.dbgprint "result:";
      Debug.dbgprintsexpr (Typing.to_sexpr r);
      let fvs = Typing.freevars r in
      Debug.dbgprint "free variables:";
      Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (function x, t -> Sexpr.Sexpr[Sexpr.Sident x; TypingType.oType_to_sexpr t]) fvs));
      let m' = backpatch m fvs in
      let b = TypingExpr.gen_exprvar () in
      let bn = TypingExpr.get_exprvar_name b in
      let eim' = {eim with eim_Expr = (ename, b) :: List.remove_assoc ename eim.eim_Expr} in
      let iem' = Id.compose iem [Id.Substitution(bn, ename)] in
      {m' with eim = eim'; iem = iem'; defs = Expr (bn, (TypingType.bindedVars t, t, r)) :: m'.defs} 

let add_expr m e = add_expr_with_env TypingExpr.empty_exprEnv m e
  
let add_expr_instance = fun m ->  function
  | ename, t, r -> 
      Debug.dbgprint "called add_expr_instance";
      Debug.dbgprint (Format.sprintf "input: %s" ename);
      Debug.dbgprintsexpr (Typing.to_sexpr r);
      let eim, iem, es = m.eim, m.iem, m.defs in
      Debug.dbgprint "typed input expr:";
      Debug.dbgprintsexpr (Typing.to_sexpr r);
      let fvs = Typing.freevars r in
      Debug.dbgprint "free variables:";
      Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (function x, t -> Sexpr.Sexpr[Sexpr.Sident x; TypingType.oType_to_sexpr t]) fvs));
      let b = Typing.gen_var t in
      let bn = Typing.varname b in
      let mss = {emptysubst with s_Expr = [(ename, t), b]} in
      let iem' = Id.compose iem [Id.Substitution(bn, ename)] in
      let m' = subst mss m in
      {m' with iem = iem'; defs = Expr (bn, ([], TypingType.OType t, r)) :: m'.defs} 

let add_typed_expr = fun m ->  function
  | ename, t, r -> 
      Debug.dbgprint "called add_typed_expr";
      Debug.dbgprint (Format.sprintf "input: %s" ename);
      Debug.dbgprintsexpr (Typing.to_sexpr r);
      let eim, iem, es = m.eim, m.iem, m.defs in
      Debug.dbgprint "typed input expr:";
      Debug.dbgprintsexpr (Typing.to_sexpr r);
      let fvs = Typing.freevars r in
      Debug.dbgprint "free variables:";
      Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (function x, t -> Sexpr.Sexpr[Sexpr.Sident x; TypingType.oType_to_sexpr t]) fvs));
      let m' = backpatch m fvs in
      let b = Typing.gen_var (TypingType.remove_quantifier t) in
      let bn = Typing.varname b in
      let be = TypingExpr.E_Variable bn in
      let iem' = Id.compose iem [Id.Substitution(bn, ename)] in
      let eim' = {eim with eim_Expr = (ename, be) :: List.remove_assoc ename eim.eim_Expr} in
      let r' = Typing.substitute_with_expr_subst eim.eim_Expr r in
      {m' with eim = eim'; iem = iem'; defs = Expr (bn, (TypingType.bindedVars t, t, r')) :: m'.defs} 

let add_mutual_recursive_expr_with_env m env ves =
  let vs, es = List.split ves in
  let ts = List.map (fun x -> TypingType.OType x) (TypingType.gen_typevars (List.length vs)) in
  let env' = TypingExpr.combine_env env (TypingExpr.ExprEnv (List.combine vs ts)) in
  let rec f rs ss env = function
    | [] -> List.map (function v, t, r -> v, TypingType.substitute_ts ss t, Typing.substitute_result_type ss r) rs
    | (v, e) :: es ->
      let t, r, ss' = Typing.typing_without_value_restriction env e in
      let ss = TypingType.compose ss' ss in
      let env' = TypingExpr.add_env env v (TypingType.OType t) in
      let env'' = TypingExpr.substitute_env ss env' in
      f ((v, Typing.value_restrict r t, r) :: rs) ss env'' es in
  List.fold_left add_typed_expr m (f [] [] env' ves)

let freeexprvars = function
  | { defs = ds } -> 
    let rec f = function
      | [] -> []
      | Type _ :: ds -> f ds
      | Expr (x, (qtvs, t, r)) :: ds -> Typing.freevars r :: f ds
    in
    List.concat (f ds)

 let freetypevars = function
  | { defs = ds } -> 
    let rec f = function
      | [] -> []
      | Type (x, (qtvs, t)) :: ds -> TypingType.freetypevars (TypingType.QType (qtvs, TypingType.OType t))  :: f ds
      | Expr (x, (qtvs, t, r)) :: ds -> List.setDiff (Typing.typevars r) qtvs :: f ds
    in
    List.concat (f ds)

let coerce_freetypevars :TypingType.oType -> t -> t = fun ot m ->
  let ftvs = freetypevars m in
  let ss = List.map (fun x -> TypingType.Substitution (x, ot)) ftvs in
  subst {emptysubst with s_Type = ss} m

let coerce_typevars :TypingType.oType -> t -> t = fun ot m ->
  let ss vs  = List.map (fun x -> TypingType.Substitution (x, ot)) vs in
  let f = function
    | Type (name, (qtvs, ot)) ->
      let ftv = TypingType.freetypevars (TypingType.OType ot) in
      Type (name, ([], TypingType.substitute (ss ftv) ot))
    | Expr (name, (ftvs, ts, r)) -> 
      let ot = TypingType.remove_quantifier ts in
      let ttv = TypingType.freetypevars (TypingType.OType ot) in
      let ts' = TypingType.OType (TypingType.substitute (ss ttv) ot) in
      let rtv = Typing.typevars r in
      let r' = Typing.substitute_result_type (ss rtv) r in
      Expr (name, ([], ts', r'))
  in
  {m with defs = List.map f m.defs}

let unused_exprvars m =
  List.setDiff (List.map (function v, t -> v) (defs_expr_cont m)) (List.map (function v, t -> v) (freeexprvars m))

let remove_expr m vs =
  {m with defs = List.filter (function Expr (v, c) -> not (List.mem v vs) | _ -> true) m.defs}

let elt_to_sexpr = function
  | Type (name, (qtvs, ot)) ->
    Sexpr.Sexpr [
      Sexpr.Sident "define-type";
      Sexpr.Sident name;
      TypingType.typeScheme_to_sexpr (TypingType.QType (qtvs, TypingType.OType ot));
    ]
  | Expr (name, (qtvs, ts, e)) ->
    Sexpr.Sexpr [
      Sexpr.Sident "define-expr";
      Sexpr.Sident name;
      TypingType.typeScheme_to_sexpr ts;
      Typing.to_sexpr e;
    ]

let gather_expr = fun m -> 
  let elts = defs_expr_cont m in
  let elts' = elts in
  let vtes = List.map (function v, (qtvs, ts, e) -> v, TypingType.remove_quantifier ts, e) elts' in
  List.fold_right Typing.gather vtes (Typing.R_Constant (Syntax.Unit, TypingType.O_Constant (Type.Unit)))

let elt_to_sexpr = function
  | Type (name, (qtvs, ot)) ->
    Sexpr.tagged_sexpr "type" [Sexpr.Sident name; Sexpr.Sexpr [Sexpr.Sexpr (List.map Sexpr.ident qtvs); TypingType.oType_to_sexpr ot]]
  | Expr (name, (qtvs, ts, r)) ->
    Sexpr.tagged_sexpr "expr" [Sexpr.Sident name; Sexpr.Sexpr [Sexpr.Sexpr (List.map Sexpr.ident qtvs); TypingType.typeScheme_to_sexpr ts; Typing.to_sexpr r]]

let to_sexpr m =
  let seim = Sexpr.Sexpr [
    Sexpr.Sident "ext->int-map";
    Sexpr.Sexpr [Sexpr.Sident "type"; TypingType.substitutions_to_sexpr m.eim.eim_Type];
    Sexpr.Sexpr [Sexpr.Sident "expr"; TypingExpr.substitutions_to_sexpr m.eim.eim_Expr];
  ] in
  let siem = Sexpr.Sexpr [
    Sexpr.Sident "int->ext-map";
    Sexpr.Sexpr [Sexpr.Sident "id"; Id.substitutions_to_sexpr m.iem];
  ] in
  let sdefs = Sexpr.Sexpr (
    Sexpr.Sident "defines" ::
    List.map elt_to_sexpr m.defs
  ) in
  Sexpr.Sexpr [seim; siem; sdefs]
 
let compose m1 m2 =
  let m2' = subst {emptysubst with s_Type = m1.eim.eim_Type; s_Expr_TE = m1.eim.eim_Expr} m2 in
  {eim = compose_eim m1.eim m2'.eim; iem = compose_iem m1.iem m2'.iem; defs = compose_defs m1.defs m2'.defs}
