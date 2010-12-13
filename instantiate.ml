open MyUtil

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
      Debug.dbgprint (Format.sprintf "expand about %s" v);
      Debug.dbgprintsexpr (Sexpr.Sexpr (List.map TypingType.oType_to_sexpr ts));
      let _, (qtvs', t', _) = (Module.def_expr m v) in
      Debug.dbgprintsexpr (Sexpr.Sexpr [Sexpr.Sexpr (List.map (fun x -> Sexpr.Sident x) qtvs'); TypingType.typeScheme_to_sexpr t']);
      let g t = 
        let ss = TypingType.unify t (TypingType.removeQuantifier t') in
        Debug.dbgprintsexpr (TypingType.substitutions_to_sexpr ss);
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
      Debug.dbgprint (Format.sprintf "exploding about %s" tv);
      let us = List.assoc tv tum in
      Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (fun t -> TypingType.oType_to_sexpr t) us));
      let rec g t = 
        Debug.dbgprint (Format.sprintf "proving about %s" tv);
        let ftvs = TypingType.typeVars t in
        Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (fun tv -> Sexpr.Sident tv) ftvs));
        let us' = List.select (List.map f ftvs) in
        Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (fun s -> Sexpr.Sexpr (List.map (fun t -> TypingType.oType_to_sexpr t) s)) us'));
        let rslt = List.map (fun u -> TypingType.substitute (List.map2 (fun tv' t' -> TypingType.Substitution (tv', t')) ftvs u) t) us' in
        Debug.dbgprintsexpr (Sexpr.Sexpr (List.map (fun t -> TypingType.oType_to_sexpr t) rslt));
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

