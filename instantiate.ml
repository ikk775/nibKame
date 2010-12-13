open MyUtil

type t = (Id.t *TypingType.oType list) list

type using = (Id.t * TypingType.oType list) list

let empty = []

let add : (Id.t * TypingType.oType) -> t -> t = fun vt um -> 
  match vt with
    | v, t ->
      if List.mem_assoc v um
      then List.map (function v', ts' when v' = v -> v, t :: ts' | _ as vts -> vts) um
      else  (v, [t]) :: um

let of_module : Module.t -> t = fun m -> 
  let um = List.fold_right add (List.map (function x, ts -> x, TypingType.removeQuantifier ts) (Module.def_exprTypes m)) empty in
  List.fold_right add (Module.exprFreeVars m) um
 
let usage : Module.t -> t -> using = fun m u -> 
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
  let sssm = List.map (List.filter ((<>) [])) (List.map f u) in
  let envm = List.concat (List.concat sssm) in
  List.fold_right (function TypingType.Substitution(tv, t) -> fun um -> add (tv, t) um) envm empty 

let usage_isvalid : using -> bool = fun um -> 
  true

let usage_expand_prototype : using -> using = fun um -> 
  Debug.dbgprint "called Module.usage_expand_prototype";
  let e tv =
    let rec f tv =
      Debug.dbgprint (Format.sprintf "exploding about %s" tv);
      let us = List.assoc tv um in
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
  List.map e (List.map (function tv, t -> tv) um)

let usage_expand : using -> using = fun um -> 
  usage_expand_prototype um

let usage_filter : using -> using = fun um -> 
  undefined ()
