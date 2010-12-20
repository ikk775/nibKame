open MyUtil

type elt =
  | Type of Id.t * (Id.t list * TypingType.oType)
  | Expr of Id.t * (Id.t list * TypingType.typeScheme * Typing.result)
(*  | Generic of Id.t * (TypingType.typeScheme * (Id.t * TypingType.oType list) list) *)

type substitutions = {
  s_Type: TypingType.substitution list;
  s_Expr: Typing.substitution list;
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

let empty:t = {eim = {eim_Type = []; eim_Expr = []}; iem = []; defs = []}

let emptyeim: extToIntMap = {eim_Type = []; eim_Expr = []}

let emptysubst: substitutions = {s_Type = []; s_Expr = []}

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

let exprEnv : t -> TypingExpr.exprEnv = fun m -> 
	let f = function
    | Expr (x, (qtvs, ts, r)) -> 
		  x, TypingType.QType (qtvs, TypingType.OType (TypingType.removeQuantifier ts))
    | Type _ -> 
	    invalid_arg "Expr requied."
  in
  TypingExpr.ExprEnv (List.map f (defs_expr m))

let rec subst : substitutions -> t -> t = fun ss m ->
  let typedefs = defs_type m in
  let typedefns = List.map elt_name typedefs in
  let exprdefs = defs_expr m in 
  let exprdefns = List.map elt_name exprdefs in
  match ss.s_Type, ss.s_Expr with
    | [], [] -> m
    | tss, [] ->
      let rec substElt = function
        | Type (tv, (qtvs, t)) ->  
          let t' = TypingType.substitute tss t in
          let ftvs = TypingType.freeTypeVars (TypingType.OType t') in
          let qtvs' = MyUtil.List.setDiff ftvs typedefns in
          Type (tv, (qtvs', t'))
        | Expr (ev, (qtvs, et, e)) -> 
          let e' = Typing.substituteResultType tss e in
          let oet = TypingType.substitute tss (TypingType.removeQuantifier et) in
          let et' = Typing.valueRestrict e' oet in
          let qtvs' = MyUtil.List.setDiff (TypingType.bindedVars et') typedefns in
          let et'' = TypingType.QType (qtvs', TypingType.OType (TypingType.removeQuantifier et')) in
          Expr (ev, (qtvs', et'', e'))
      in
      subst {ss with s_Type = []} {m with defs = List.map substElt m.defs}
    | _, ess -> 
      let rec substElt = function
        | Type _ as t-> t
        | Expr (ev, (qtvs, et, r)) -> 
          let r' = Typing.substitute ess r in
          Expr (ev, (qtvs, et, r'))
      in
      subst {ss with s_Expr = []} {m with defs = List.map substElt m.defs}
  
let addType = fun m -> 
  function
    | tname, (tvs, t) ->
      let eim, iem, es = m.eim, m.iem, m.defs in
      let b = TypingType.genTypeVar () in
      let bn = TypingType.getOTypeVariableName b in
      let tvbs = TypingType.genTypeVars (List.length tvs) in
      let tvbsn = List.map TypingType.getOTypeVariableName tvbs in
      let eim' = {eim with eim_Type =TypingType.composite eim.eim_Type [TypingType.Substitution(tname, b)]} in
      let eim'' = {eim' with eim_Type =TypingType.composite eim.eim_Type (List.map2 (fun f t -> TypingType.Substitution(f, t)) tvs tvbs)} in
      let iem' = Id.compose iem (Id.Substitution(bn, tname) :: List.map2 (fun f t -> Id.Substitution(f, t)) tvbsn tvs) in
      let t' = TypingType.substitute eim''.eim_Type t in
      let substtoname tvn = TypingType.getOTypeVariableName (TypingType.substitute eim''.eim_Type (TypingType.O_Variable tvn)) in
      {m with eim = eim'; iem = iem'; defs = (Type (bn, (List.map substtoname tvs , t')) :: es)}

(* top-level let 相当 *)
let addExpr = fun m -> 
  function
    | ename, e -> 
      Debug.dbgprint "called addExpr";
      Debug.dbgprint (Format.sprintf "input: %s" ename);
      Debug.dbgprintsexpr (TypingExpr.to_sexpr e);
      let eim, iem, es = m.eim, m.iem, m.defs in
      let e' = TypingExpr.substituteExpr eim.eim_Expr e in
      let t, r = Typing.typing (exprEnv m) e' in
      Debug.dbgprint "typed input expr";
      Debug.dbgprint "type:";
      Debug.dbgprintsexpr (TypingType.typeScheme_to_sexpr t);
      Debug.dbgprint "result:";
      Debug.dbgprintsexpr (Typing.result_to_sexpr r);
      let fvs = Typing.freeVars r in
      Debug.dbgprint "free variables:";
      Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (function x, t -> Sexpr.Sexpr[Sexpr.Sident x; TypingType.oType_to_sexpr t]) fvs));
      let rec f m = function
        | [] -> m
        | (fv, ot) :: fvs -> 
          Debug.dbgprint (Format.sprintf "backpatching %s" fv);
          let _, (qtvs', t', r') = def_expr m fv in
          let ot' = TypingType.removeQuantifier t' in
          Debug.dbgprintsexpr (TypingType.oType_to_sexpr ot');
          let ss = TypingType.renew ot' ot in
          Debug.dbgprint "subst:";
          Debug.dbgprintsexpr (TypingType.substitutions_to_sexpr ss);
          let ss' = TypingType.domainDiff ss qtvs' in
          Debug.dbgprint "free variables:";
          Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (fun x -> Sexpr.Sident x) qtvs'));
          Debug.dbgprint "free-variable-removed subst:";
          Debug.dbgprintsexpr (TypingType.substitutions_to_sexpr ss');
          let m' = subst {s_Type = ss'; s_Expr = []} m in
          f m' fvs
      in
      let m' = f m fvs in
      let b = TypingExpr.genExprVar () in
      let bn = TypingExpr.getExprVarName b in
      let eim' = {eim with eim_Expr = (ename, b) :: List.remove_assoc ename eim.eim_Expr} in
      let iem' = Id.compose iem [Id.Substitution(bn, ename)] in
      {m' with eim = eim'; iem = iem'; defs = Expr (bn, (TypingType.bindedVars t, t, r)) :: m'.defs} 

let addExprInstance = fun m ->  function
  | ename, t, r -> 
      Debug.dbgprint "called addExprInstance";
      Debug.dbgprint (Format.sprintf "input: %s" ename);
      Debug.dbgprintsexpr (Typing.result_to_sexpr r);
      let eim, iem, es = m.eim, m.iem, m.defs in
      Debug.dbgprint "typed input expr:";
      Debug.dbgprintsexpr (Typing.result_to_sexpr r);
      let fvs = Typing.freeVars r in
      Debug.dbgprint "free variables:";
      Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (function x, t -> Sexpr.Sexpr[Sexpr.Sident x; TypingType.oType_to_sexpr t]) fvs));
      let b = Typing.genVar t in
      let bn = Typing.varName b in
      let mss = {emptysubst with s_Expr = [(ename, t), b]} in
      let iem' = Id.compose iem [Id.Substitution(bn, ename)] in
      let m' = subst mss m in
      {m' with iem = iem'; defs = Expr (bn, ([], TypingType.OType t, r)) :: m'.defs} 

let exprFreeVars = function
  | { defs = ds } -> 
    let rec f = function
      | [] -> []
      | Type _ :: ds -> f ds
      | Expr (x, (qtvs, t, r)) :: ds -> Typing.freeVars r :: f ds
    in
    List.concat (f ds)

 let typeFreeVars = function
  | { defs = ds } -> 
    let rec f = function
      | [] -> []
      | Type (x, (qtvs, t)) :: ds -> TypingType.freeTypeVars (TypingType.QType (qtvs, TypingType.OType t))  :: f ds
      | Expr (x, (qtvs, t, r)) :: ds -> List.setDiff (Typing.typeVars r) qtvs :: f ds
    in
    List.concat (f ds)

let coerceFreeTypeVars :TypingType.oType -> t -> t = fun ot m ->
  let ftvs = typeFreeVars m in
  let ss = List.map (fun x -> TypingType.Substitution (x, ot)) ftvs in
  subst {emptysubst with s_Type = ss} m

let unusedExprVars m =
  List.setDiff (List.map (function v, t -> v) (defs_expr_cont m)) (List.map (function v, t -> v) (exprFreeVars m))

let removeExpr m vs =
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
  let sdefs = Sexpr.Sexpr [
    Sexpr.Sident "defines";
  ] in
  Sexpr.Sexpr [seim; siem; sdefs]
  
