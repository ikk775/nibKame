open MyUtil
   
type elt = Id.t * (Id.t list * TypingType.typeScheme * Typing.result)

type intToExtMap = Id.substitution list

type t = {
  iem: intToExtMap;
  defs: elt list
}

type ev_usage = (Id.t *TypingType.oType list) list

type tv_usage = (Id.t * TypingType.oType list) list

let empty = []

let add : (Id.t * TypingType.oType) -> ev_usage -> ev_usage = fun vt eum -> 
  match vt with
    | v, t ->
      if List.mem_assoc v eum
      then List.map (function v', ts' when v' = v -> v, t :: ts' | _ as vts -> vts) eum
      else  (v, [t]) :: eum

let of_module : Module.t -> ev_usage = fun m -> 
  let um = List.fold_right add (List.map (function x, ts -> x, TypingType.removeQuantifier ts) (Module.def_exprTypes m)) empty in
  List.fold_right add (Module.exprFreeVars m) um
 
let usage : Module.t -> ev_usage -> tv_usage = fun m eum -> 
  Debug.dbgprint "called Module.usage";
  let f = function v, ts -> 
      let _, (qtvs', t', _) = (Module.def_expr m v) in
      let g t = 
        let ss = TypingType.unify t (TypingType.removeQuantifier t') in
        List.append (TypingType.domainRestrict ss qtvs') (List.filter (function TypingType.Substitution (_, TypingType.O_Variable x) when List.mem x qtvs' -> true | _ -> false) ss)
      in
      List.map g ts
  in
  let sssm = List.map (List.filter ((<>) [])) (List.map f eum) in
  let envm = List.concat (List.concat sssm) in
  List.fold_right (function TypingType.Substitution(tv, t) -> fun um -> add (tv, t) um) envm empty 

let usage_isvalid : tv_usage -> bool = fun tum -> 
  true

let usage_expand_prototype : tv_usage -> tv_usage = fun tum -> 
  Debug.dbgprint "called Module.usage_expand_prototype";
  let e tv =
    let rec f tv =
      let us = List.assoc tv tum in
      let rec g t = 
        let ftvs = TypingType.typeVars t in
        let us' = List.select (List.map f ftvs) in
        let rslt = List.map (fun u -> TypingType.substitute (List.map2 (fun tv' t' -> TypingType.Substitution (tv', t')) ftvs u) t) us' in
        rslt
      in
      List.concat (List.map (fun t -> if TypingType.typeVars t <> [] then g t else [t]) us)
    in
  tv, f tv
  in
  List.map e (List.map (function tv, t -> tv) tum)

let usage_expand : tv_usage -> tv_usage = fun tum -> 
  usage_expand_prototype tum

let usage_filter : tv_usage -> tv_usage = fun tum -> 
  tum

