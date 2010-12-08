open MyUtil

type template =
  | Type of Id.t * (Id.t list * TypingType.oType)
  | Expr of Id.t * (Id.t list * TypingType.typeScheme * Typing.result)

type elt = template

type substitutions = {
  s_Type: TypingType.substitution list;
  s_Expr: TypingExpr.substitution list;
  }

type extToIntMap = substitutions

type intToExtMap = Id.substitution list

type t = {
  eim: extToIntMap;
  iem: intToExtMap;
  defs: elt list
}

let empty:t = {eim = {s_Type = []; s_Expr = []}; iem = []; defs = []}

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

let def_tail : t -> elt -> t = fun m def -> 
  {m with defs = def :: defs m}
 
let def_head : t -> elt -> t = fun m def -> 
  {m with defs = List.append (defs m) [def]}

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
    | tss, ess -> 
      undefined ()
  
let add : elt -> t -> t = fun e m -> 
  match e with
    | Type (tname, (tvs, t)) ->
      let eim, iem, es = m.eim, m.iem, m.defs in
      let b = TypingType.genTypeVar () in
      let bn = TypingType.getOTypeVariableName b in
      let tvbs = TypingType.genTypeVars (List.length tvs) in
      let tvbsn = List.map TypingType.getOTypeVariableName tvbs in
      let eim' = {eim with s_Type =TypingType.composite eim.s_Type [TypingType.Substitution(tname, b)]} in
      let eim'' = {eim' with s_Type =TypingType.composite eim.s_Type (List.map2 (fun f t -> TypingType.Substitution(f, t)) tvs tvbs)} in
      let iem' = Id.compose iem (Id.Substitution(bn, tname) :: List.map2 (fun f t -> Id.Substitution(f, t)) tvbsn tvs) in
      let t' = TypingType.substitute eim''.s_Type t in
      let substtoname tvn = TypingType.getOTypeVariableName (TypingType.substitute eim''.s_Type (TypingType.O_Variable tvn)) in
      {m with eim = eim'; iem = iem'; defs = (Type (bn, (List.map substtoname tvs , t')) :: es)}
   |  Expr (ename, (tvs, rt, es)) ->
      undefined ()