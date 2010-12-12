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
 
let expand (*: Module.t -> t -> t*) = fun m u -> 
  let f = function v, ts -> 
      let g t = 
        let _, (qtvs', t', _) = (Module.def_expr m v) in
        TypingType.domainRestrict (TypingType.unify t (TypingType.removeQuantifier t')) qtvs'
      in
      List.map g ts
  in
  let sssm = List.map f u
  in
  sssm
