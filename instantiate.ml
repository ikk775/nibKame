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
  Debug.dbgprint "called Module.expand";
  let f = function v, ts -> 
      Debug.dbgprint (Format.sprintf "expand about %s" v);
      Debug.dbgprintsexpr (Sexpr.Sexpr (List.map TypingType.oType_to_sexpr ts));
      let _, (qtvs', t', _) = (Module.def_expr m v) in
      Debug.dbgprintsexpr (Sexpr.Sexpr [Sexpr.Sexpr (List.map (fun x -> Sexpr.Sident x) qtvs'); TypingType.typeScheme_to_sexpr t']);
      let g t = 
        let ss = TypingType.unify t (TypingType.removeQuantifier t') in
        Debug.dbgprintsexpr (TypingType.substitutions_to_sexpr ss);
        TypingType.domainRestrict ss qtvs'
      in
      List.map g ts
  in
  let sssm = List.map (List.filter ((<>) [])) (List.map f u) in
  let envm = List.concat (List.concat sssm) in
  List.fold_right (function TypingType.Substitution(tv, t) -> fun um -> add (tv, t) um) envm empty 
