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

let defs : t -> elt list = fun m ->
  let es = m.defs in
  List.rev es

let def_tail : t -> elt -> t = fun m def -> 
  {m with defs = def :: defs m}
 
let def_head : t -> elt -> t = fun m def -> 
  {m with defs = List.append (defs m) [def]}
 
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