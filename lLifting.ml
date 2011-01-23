open MyUtil

module R = Typing
module T = TypingType
module E = TypingExpr

let max_iter = 1000

type t =
  | Constant of Syntax.lit * TypingType.oType
  | Variable of Id.t * TypingType.oType
(*  | Fun of (Id.t * TypingType.oType) list * t *)
  | Apply of t * t list
  | Tuple of t list * TypingType.oType
  | Vector of t list * TypingType.oType
  | If of t * t * t
  | Let of (Id.t * TypingType.oType) * t * t
  | LetFun of (Id.t * TypingType.oType) * (Id.t * TypingType.oType) list * t * t
  | Match of t * (pattern * t option * t) list
  | External of Id.t * TypingType.oType
and pattern =
  | P_Constant of Syntax.lit * TypingType.oType
  | P_Variable of Id.t option * TypingType.oType
  | P_Constructor of Id.t * TypingType.oType
  | P_Apply of (pattern * pattern) * TypingType.oType
  | P_And of (pattern * pattern) * TypingType.oType
  | P_Or of (pattern * pattern) * TypingType.oType (* Both patterns must have a same set of variables. And each variable has same type across the patterns. *)
  | P_Not of pattern * TypingType.oType
  | P_Tuple of pattern list * TypingType.oType
  | P_Vector of pattern list * TypingType.oType
and clause = pattern * t option * t

type fundef = {
  fun_name : Id.t * TypingType.oType;
  args : (Id.t * TypingType.oType) list;
  body : t }

type topvar = {
  var_name : Id.t * TypingType.oType;
  expr : t
}

type topDecl =
  | FunDecl of fundef
  | VarDecl of topvar

type topDecls = topDecl list (* in a reverse order *)

let gen_var_num = ref 0

let gen_varname () =
  gen_var_num := !gen_var_num + 1;
  Format.sprintf "$l:%d" !gen_var_num

let gen_var t =
  let b = gen_varname () in
  Variable (b, t)

let rec gen_varnames n =
  List.iter_list n gen_varname

let varname = function
  | Variable (v, t) -> v
  | _ -> invalid_arg "varname"

let is_variable = function
  | Variable (v, t) -> true
  | _ -> false

let rec freevars r =
  let f = function
    | External _ -> []
    | Constant _ -> []
    | Variable (v, t) -> [v, t]
    | Apply (e, es) -> freevars e @ List.concat (List.map freevars es)
    | Tuple (es, t) -> List.concat (List.map freevars es)
    | Vector (es, t) -> List.concat (List.map freevars es)
    | If (e1, e2, e3) -> List.concat (List.map freevars [e1; e2; e3])
    | Let ((v, t), e1, e2) -> freevars e1 @ (List.setDiff (List.unique (freevars e2)) [v, t])
    | LetFun ((v, t), args, e1, e2) -> 
      let vs = v :: List.map fst args in
      List.filter (function v', t' -> not (List.mem v' vs)) (List.unique (freevars e1 @ freevars e2)) 
    | Match _ -> undefined ()
  in
  List.unique (f r)

let rec substitute ss r =
  let subst = substitute ss in
  let subst' v = substitute (List.remove_assoc v ss) in
  let subst'' vs = substitute (List.filter (function v, t -> not (List.mem v vs)) ss) in
  match r with
    | External _ -> r
    | Constant _ -> r
    | Variable (v, t) when List.mem_assoc v ss -> List.assoc v ss
    | Variable _ -> r
    | Apply (e, es) -> Apply (subst e, List.map subst es)
    | Tuple (es, t) -> Tuple (List.map subst es, t)
    | Vector (es, t) -> Vector (List.map subst es, t)
    | If (e1, e2, e3) -> If (subst e1, subst e2, subst e3)
    | Let ((v, t), e1, e2) -> Let ((v, t), subst e1, subst' v e2)
    | LetFun ((v, t), args, e1, e2) -> 
      let vs = v :: List.map fst args in
      LetFun ((v, t), args, subst'' vs e1, subst'' vs e2)
    | Match _ -> undefined ()

let rec compose_subst xs ys =
  let subst_v x ys = (List.map (fun y -> match y with fv, te -> (fv ,substitute [x] te)) ys) in
  match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, ys ->
      if (List.exists (fun y -> 
        match x, y with
          | (fvx, _), (fvy, _) -> fvx = fvy) ys)
      then compose_subst xs (subst_v x ys)
      else compose_subst xs (x :: subst_v x ys)

let rec substitute_varname ss r =
  let subst = substitute_varname ss in
  let subst' v = substitute_varname (List.remove_assoc v ss) in
  let subst'' vs = substitute_varname (List.filter (function v, t -> not (List.mem v vs)) ss) in
  match r with
    | External _ -> r
    | Constant _ -> r
    | Variable (v, t) when List.mem_assoc v ss -> Variable (List.assoc v ss, t)
    | Variable _ -> r
    | Apply (e, es) -> Apply (subst e, List.map subst es)
    | Tuple (es, t) -> Tuple (List.map subst es, t)
    | Vector (es, t) -> Vector (List.map subst es, t)
    | If (e1, e2, e3) -> If (subst e1, subst e2, subst e3)
    | Let ((v, t), e1, e2) -> Let ((v, t), subst e1, subst' v e2)
    | LetFun ((v, t), args, e1, e2) -> 
      let vs = v :: List.map fst args in
      LetFun ((v, t), args, subst'' vs e1, subst'' vs e2)
    | Match _ -> undefined ()

let rec substitute_decl_name ss defs =
  let f = function
    | FunDecl {fun_name = (v, t); args = vts; body = e} ->
      let v' = if List.mem_assoc v ss then List.assoc v ss else v in
      let argvs = List.map fst vts in
      let ss' = List.filter (function v', t' -> List.mem v' argvs) ss in
      FunDecl {fun_name = (v', t); args = vts; body = substitute_varname ss' e}
    | VarDecl {var_name = (v, t); expr = e} ->
      let v' = if List.mem_assoc v ss then List.assoc v ss else v in
      VarDecl {var_name = (v', t); expr = substitute_varname ss e}
  in
  List.map f defs

let rec add_type_args_rev t = function
  | [] -> t
  | t' :: ts' -> add_type_args_rev (T.O_Fun (t', t)) ts'
let add_type_args t ts = add_type_args_rev t (List.rev ts) 

let rec of_typint_result r =
  let f = of_typint_result in
  let rec unfold_apply es = function
    | R.R_Apply (e1, e2) -> 
      unfold_apply (e2 :: es) e1
    | e -> es, e
  in
  let rec unfold_fun_rev vts = function
    | R.R_Fun ((v, t), e) -> 
      unfold_fun_rev ((v, t) :: vts) e
    | e -> vts, e
  in
  let unfold_fun vts r =
    let vts, r' = unfold_fun_rev vts r in
    (List.rev vts), r'
  in
  match r with
    | R.R_Constant (lit, t) -> Constant (lit, t)
    | R.R_External (v, t) -> External (v, t)
    | R.R_Variable (v, t) -> Variable (v, t)
    | R.R_Tuple (rs, t) -> (Tuple (List.map f rs, t))
    | R.R_Vector (rs, t) -> (Vector (List.map f rs, t))
    | R.R_If (e1, e2, e3) -> If (f e1, f e2, f e3)
    | R.R_Let ((v, t), e1, e2) ->
      begin match e1 with
        | R.R_Fix ((v', t'), e, t'') ->
          let e' = R.substitute_varname [v', v] e in
          let vts, e'' = unfold_fun [] e' in
          let t' = T.O_Fun (t, R.result_type e) in
          begin match vts with
            | [] -> failwith "Fix operator must take fun."
            | _ -> 
              LetFun ((v, t), vts, f e'', f e2)
          end
        | _ -> 
          let vts, e1' = unfold_fun [] e1 in
          begin match vts with
            | [] -> Let ((v, t), f e1, f e2)
            | _ -> 
              LetFun ((v, t), vts, f e1', f e2)
          end
      end
    | R.R_Fun ((v, t), e) -> 
      let bn = gen_varname () in
      let vts, e' = unfold_fun [] e in
      begin match (v, t) :: vts with
        | [] -> failwith ""
        | vts' -> 
          let t' = add_type_args (R.result_type e) (List.map snd vts') in
          LetFun ((bn, t'), vts', f e', Variable (bn, t'))
      end
    | R.R_Fix ((v, t), e, t') -> 
      let bn = gen_varname () in
      let e' = R.substitute_varname [v, bn] e in
      let vts, e' = unfold_fun [] e' in
      begin match (v, t) :: vts with
        | [] -> failwith ""
        | vts' -> 
          let t' = add_type_args (R.result_type e) (List.map snd vts') in
          LetFun ((bn, t'), vts', f e', Variable (bn, t'))
      end
    | R.R_Apply (e1, e2) -> 
      let vts, e1' = unfold_apply [] e1 in
      begin match e2 :: vts with
        | [] -> failwith ""
        | es' -> 
          Apply (f e1', List.map f es')
      end
    | R.R_Match _ -> undefined ()

let vts_to_vars vts =
  List.map (function v, t -> Variable (v, t)) vts

let rec uncover ss known r =
  let f = uncover ss known in
  let f' v = uncover (List.remove_assoc v ss) (List.setDiff known [v]) in
  match r with
    | External _ -> r
    | Constant _ -> r
    | Variable _ -> substitute ss r
    | LetFun ((v, t), args, e1, e2) ->
      let fvts = freevars e1 in
      let vs = v :: List.map fst args in
      let fvts' = List.filter (function v', t' -> not (List.mem v' vs)) fvts in
      let fts = List.map snd fvts' in
      let ss = compose_subst ss [v, Apply (Variable (v, t), vts_to_vars fvts')] in
      let known' = List.setDiff known vs in
      let e1' = uncover ss known' e1 in
      let e2' = uncover ss known' e2 in
      LetFun ((v, add_type_args t fts), fvts' @ args, e1', e2')
    | Apply (e, es) -> Apply (f e, List.map f es)
    | Tuple (es, t) -> Tuple (List.map f es, t)
    | Vector (es, t) -> Vector (List.map f es, t)
    | If (e1, e2, e3) ->  If (f e1, f e2, f e3)
    | Let ((v, t), e1, e2) -> Let ((v, t), f e1, f' v e2)
    | Match _ -> undefined ()

let rec fuse' ss r =
  let f = fuse' ss in
  let f' v = fuse' (List.remove_assoc v ss) in
  let f'' vs = fuse' (List.filter (function v, t -> not (List.mem v vs)) ss) in
  let f''' v t v' vts =
    let vs = List.map fst vts in
    let ss = List.filter (function v, t -> not (List.mem v vs)) ss in
    let ss' = (v', Apply(Variable (v, t), List.map (function v, t -> Variable (v, t)) vts)) :: ss in
    fuse' ss'
  in
  match r with
    | External _ -> r
    | Constant _ -> r
    | Variable _ -> substitute ss r
    | Apply (Apply(e, es), es') ->
      f (Apply (e, es @ es'))
    | Apply (e, []) -> f e
    | Apply (e, es) -> Apply (f e, List.map f es)
    | Tuple (es, t) -> Tuple (List.map f es, t)
    | Vector (es, t) -> Vector (List.map f es, t)
    | If (e1, e2, e3) -> If (f e1, f e2, f e3)
    | Let ((v, t), e1, Variable (v', t')) when v = v' -> e1
    | Let ((v, t), Variable (v', t'), e2) -> substitute_varname [v, v'] e2
    | Let ((v, t), e1, e2) -> Let ((v, t), f e1, f' v e2)
    | LetFun ((v, t), args, LetFun ((v', t'), args', e1', Variable (v'', t'')), e2) when v' = v'' ->
      LetFun ((v, t), args @ args', f''' v t v' args e1', f e2)
    | LetFun ((v, t), args, e1, e2) ->
      let vs = v :: List.map fst args in
      LetFun ((v, t), args, f'' vs e1, f'' vs e2)
    | Match _ -> undefined ()

let fuse l =
  let rec f n l =
    let l' = fuse' [] l in
    if n < 0 || l = l'
    then l
    else f (n - 1) l'
  in
  f max_iter l
    


let rec get_type = function
  | Constant (l, t) -> t
  | Variable (v, t) -> t
  | Apply(e1, e2) ->
    begin match get_type e1 with
      | TypingType.O_Fun(ft, tt) -> tt
      | _ -> invalid_arg ""
    end
  | Tuple (es, t) -> t
  | Vector (es, t) -> t
  | If (e1, e2, e3) -> get_type e2
  | Let ((v, t), e1, e2) -> get_type e2
  | LetFun ((v, t), args, e1, e2) -> get_type e2
  | External (v, t) -> t
  | Match (_, (_, _, e) :: _) ->
    get_type e
  | Match (_,  _) ->
    failwith "Empty clause."


let rec lift r =
  let defs = ref [] in
  let rec f r = match r with
    | External _ -> r
    | Constant _ -> r
    | Variable _ -> r
    | LetFun ((v, t), args, e1, e2) when freevars r = [] ->
      let bn = gen_varname () in
      defs := FunDecl {fun_name = (bn, t); args = args; body = e1} :: !defs;
      f (substitute_varname [v, bn] e2)
    | LetFun ((v, t), args, e1, e2) -> invalid_arg "lift"
    | Apply (e, es) -> Apply (f e, List.map f es)
    | Tuple (es, t) -> Tuple (List.map f es, t)
    | Vector (es, t) -> Vector (List.map f es, t)
    | If (e1, e2, e3) ->  If (f e1, f e2, f e3)
    | Let ((v, t), e1, e2) -> Let ((v, t), f e1, f e2)
    | Match _ -> undefined ()
  in
  let r' = f r in
  r', defs

let vt_to_sexprs = function v, t -> [Sexpr.Sident v; T.oType_to_sexpr t]
let vt_to_sexpr = function v, t -> Sexpr.Sexpr (vt_to_sexprs (v, t))
let lt_to_sexprs = function Id.L v, t -> [Sexpr.Sident v; T.oType_to_sexpr t]
let lt_to_sexpr = function v, t -> Sexpr.Sexpr (lt_to_sexprs (v, t))

let rec to_sexpr = function
  | Constant (lit, t) -> Sexpr.tagged_sexpr "l:const" [Syntax.lit_to_sexpr lit; T.oType_to_sexpr t]
  | External (v, t) -> Sexpr.tagged_sexpr "l:external" (vt_to_sexprs (v, t))
  | Variable (v, t) -> Sexpr.tagged_sexpr "l:var" (vt_to_sexprs (v, t))
  | Apply (e, es) -> Sexpr.tagged_sexpr "l:apply" (List.map to_sexpr (e :: es))
  | Tuple (es, t) -> Sexpr.tagged_sexpr "l:tuple" (T.oType_to_sexpr t :: List.map to_sexpr es)
  | Vector (es, t) -> Sexpr.tagged_sexpr "l:vector" (T.oType_to_sexpr t :: List.map to_sexpr es)
  | If (e1, e2, e3) -> Sexpr.tagged_sexpr "l:if" (List.map to_sexpr [e1; e2; e3])
  | Let ((v, t), e1, e2) -> Sexpr.tagged_sexpr "l:let" (vt_to_sexpr (v, t) :: List.map to_sexpr [e1; e2])
  | LetFun ((v, t), args, e1, e2) -> Sexpr.tagged_sexpr "l:letfun" [Sexpr.Sexpr (vt_to_sexpr (v, t) :: List.map vt_to_sexpr args); Sexpr.Sexpr (List.map to_sexpr [e1; e2])]
  | Match _ -> undefined ()

let to_decl name = function
  | LetFun ((v, t), args, e, Variable (v', t')) when v = v' -> 
    FunDecl {fun_name = (name, t); args = args; body = substitute_varname [v, name] e}
  | LetFun _ as r-> failwith (Printf.sprintf "Unexpected argument. Maybe, lambda lifting is mistaken. Got: %s" (Sexpr.to_string (to_sexpr r)))
  | e ->
    VarDecl {var_name = (name, get_type e); expr = e}

let convert known v l =
  Debug.dbgprint (Printf.sprintf "lambda lifting: %s" v);
  Debug.dbgprintsexpr (to_sexpr l);
  let l' = uncover [] known l in
  Debug.dbgprint "uncovered:";
  Debug.dbgprintsexpr (to_sexpr l');
  let l'' = fuse l' in
  Debug.dbgprint "fused:";
  Debug.dbgprintsexpr (to_sexpr l'');
  let l''', defs = lift l'' in
  Debug.dbgprint "lifted:";
  Debug.dbgprintsexpr (to_sexpr l''');
  match l''' with
    | Variable (v', t') -> 
      substitute_decl_name [v', v] !defs
    | _ -> 
      match get_type l''' with
        | T.O_Fun _ as t-> 
          let ts = T.fun_args t in
          let bns = gen_varnames (List.length ts) in
          let bvts = List.combine bns ts in
          let bs = List.map (function v, t -> Variable (v, t)) bvts in
          to_decl v (fuse (LetFun ((v, t), bvts, Apply (l''', bs), Variable (v, t))))  :: !defs
        | _ -> 
          to_decl v l'''  :: !defs

let from_module m =
  let defs = Module.defs_expr_cont m in
  let vrs = List.map (function v, (qtvs, ts, r) -> v, r) defs in
  let vls = List.map (function v, r -> v, of_typint_result r) vrs in
  let vs = List.map fst vls in
  List.concat (List.map (function v, l -> convert vs v l) vls)
