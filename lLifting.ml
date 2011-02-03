open MyUtil

module R = Typing
module T = TypingType
module E = TypingExpr

let max_iter = 1000

type t =
  | Constant of Syntax.lit
  | Variable of Id.t
(*  | Fun of (Id.t * TypingType.oType) list * t *)
  | Apply of t * t list
  | Tuple of t list
  | Vector of t list
  | If of t * t * t
  | Let of (Id.t) * t * t
  | LetFun of (Id.t) * (Id.t) list * t * t
  | Match of t * (pattern * t option * t) list
  | External of Id.t * TypingType.oType
and pattern =
  | P_Constant of Syntax.lit
  | P_Variable of Id.t option
  | P_Constructor of Id.t
  | P_Apply of (pattern * pattern)
  | P_And of (pattern * pattern)
  | P_Or of (pattern * pattern)(* Both patterns must have a same set of variables. And each variable has same type across the patterns. *)
  | P_Not of pattern
  | P_Tuple of pattern list
  | P_Vector of pattern list
and clause = pattern * t option * t

type fundef = {
  fun_name : Id.t;
  args : (Id.t) list;
  body : t }

type topDecl = fundef

type topDecls = topDecl list (* in a reverse order *)

let gen_var_num = ref 0

let gen_varname () =
  gen_var_num := !gen_var_num + 1;
  Format.sprintf "$l:%d" !gen_var_num

let gen_var t =
  let b = gen_varname () in
  Variable b

let rec gen_varnames n =
  List.iter_list n gen_varname

let varname = function
  | Variable v -> v
  | _ -> invalid_arg "varname"

let is_variable = function
  | Variable v -> true
  | _ -> false

let rec freevars r =
  let f = function
    | External _ -> []
    | Constant _ -> []
    | Variable v -> [v]
    | Apply (e, es) -> freevars e @ List.concat (List.map freevars es)
    | Tuple (es) -> List.concat (List.map freevars es)
    | Vector (es) -> List.concat (List.map freevars es)
    | If (e1, e2, e3) -> List.concat (List.map freevars [e1; e2; e3])
    | Let (v, e1, e2) -> freevars e1 @ (List.setDiff (List.unique (freevars e2)) [v])
    | LetFun ((v), args, e1, e2) -> 
      List.setDiff (List.unique (freevars e1 @ freevars e2)) (v :: args)
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
    | Variable v when List.mem_assoc v ss -> List.assoc v ss
    | Variable _ -> r
    | Apply (e, es) -> Apply (subst e, List.map subst es)
    | Tuple (es) -> Tuple (List.map subst es)
    | Vector (es) -> Vector (List.map subst es)
    | If (e1, e2, e3) -> If (subst e1, subst e2, subst e3)
    | Let ((v), e1, e2) -> Let ((v), subst e1, subst' v e2)
    | LetFun ((v), args, e1, e2) -> 
      let vs = v :: args in
      LetFun ((v), args, subst'' vs e1, subst'' vs e2)
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
    | Variable (v) when List.mem_assoc v ss -> Variable (List.assoc v ss)
    | Variable _ -> r
    | Apply (e, es) -> Apply (subst e, List.map subst es)
    | Tuple (es) -> Tuple (List.map subst es)
    | Vector (es) -> Vector (List.map subst es)
    | If (e1, e2, e3) -> If (subst e1, subst e2, subst e3)
    | Let ((v), e1, e2) -> Let ((v), subst e1, subst' v e2)
    | LetFun ((v), args, e1, e2) -> 
      let vs = v :: args in
      LetFun ((v), args, subst'' vs e1, subst'' vs e2)
    | Match _ -> undefined ()

let rec substitute_decl_name ss defs =
  let f = function
    | {fun_name = (v); args = argvs; body = e} ->
      let v' = if List.mem_assoc v ss then List.assoc v ss else v in
      let ss' = List.filter (function v', t' -> List.mem v' argvs) ss in
      {fun_name = (v'); args = argvs; body = substitute_varname ss' e}
  in
  List.map f defs

let rec add_type_args_rev t = function
  | [] -> t
  | t' :: ts' -> add_type_args_rev (T.O_Fun (t', t)) ts'
let add_type_args t ts = add_type_args_rev t (List.rev ts) 

let rec of_typint_expr r =
  let f = of_typint_expr in
  let rec unfold_apply es = function
    | E.E_Apply (e1, e2) -> 
      unfold_apply (e2 :: es) e1
    | e -> es, e
  in
  let rec unfold_fun_rev vs = function
    | E.E_Fun (v, e) -> 
      unfold_fun_rev (v :: vs) e
    | e -> vs, e
  in
  let unfold_fun vs r =
    let vs, r' = unfold_fun_rev vs r in
    (List.rev vs), r'
  in
  match r with
    | E.E_Constant (lit) -> Constant (lit)
    | E.E_External (v, t) -> External (v, t)
    | E.E_Variable (v) -> Variable (v)
    | E.E_Tuple (rs) -> (Tuple (List.map f rs))
    | E.E_Vector (rs) -> (Vector (List.map f rs))
    | E.E_If (e1, e2, e3) -> If (f e1, f e2, f e3)
    | E.E_Let ((v), e1, e2) ->
      begin match e1 with
        | E.E_Fix ((v'), e) ->
          let e' = E.substitute_varname [v', v] e in
          let vs, e'' = unfold_fun [] e' in
          begin match vs with
            | [] -> failwith "Fix operator must take fun."
            | _ -> 
              LetFun (v, vs, f e'', f e2)
          end
        | _ -> 
          let vs, e1' = unfold_fun [] e1 in
          begin match vs with
            | [] -> Let (v, f e1, f e2)
            | _ -> 
              LetFun (v, vs, f e1', f e2)
          end
      end
    | E.E_Fun ((v), e) -> 
      let bn = gen_varname () in
      let vts, e' = unfold_fun [] e in
      begin match v :: vts with
        | [] -> failwith ""
        | vs' -> 
          LetFun (bn, vs', f e', Variable bn)
      end
    | E.E_Fix ((v), e) -> 
      let vs, e' = unfold_fun [] e in
      LetFun (v, vs, f e', Variable v)
    | E.E_Apply (e1, e2) -> 
      let vts, e1' = unfold_apply [] e1 in
      begin match e2 :: vts with
        | [] -> failwith ""
        | es' -> 
          Apply (f e1', List.map f es')
      end
    | E.E_Match _ -> undefined ()
let vts_to_vars vts =
  List.map (function v, t -> Variable (v)) vts

let vs_to_vars vs =
  List.map (fun v -> Variable v) vs
  
let rec uncover ss known r =
  let f = uncover ss known in
  let f' v = uncover (List.remove_assoc v ss) (List.setDiff known [v]) in
  match r with
    | External _ -> r
    | Constant _ -> r
    | Variable _ -> substitute ss r
    | LetFun (v, vs, e1, e2) ->
      let fvs = freevars e1 in
      let fvs' = List.filter (fun v' -> not (List.mem v' vs)) fvs in
      let ss = compose_subst ss [v, Apply (Variable (v), vs_to_vars fvs')] in
      let known' = List.setDiff known vs in
      let e1' = uncover ss known' e1 in
      let e2' = uncover ss known' e2 in
      LetFun (v, fvs' @ vs, e1', e2')
    | Apply (e, es) -> Apply (f e, List.map f es)
    | Tuple (es) -> Tuple (List.map f es)
    | Vector (es) -> Vector (List.map f es)
    | If (e1, e2, e3) ->  If (f e1, f e2, f e3)
    | Let ((v), e1, e2) -> Let ((v), f e1, f' v e2)
    | Match _ -> undefined ()

let rec fuse' ss r =
  let f = fuse' ss in
  let f' v = fuse' (List.remove_assoc v ss) in
  let f'' vs = fuse' (List.filter (function v, t -> not (List.mem v vs)) ss) in
  let f''' v v' vs =
    let ss = List.filter (function v, t -> not (List.mem v vs)) ss in
    let ss' = (v', Apply(Variable (v), List.map (fun v -> Variable (v)) vs)) :: ss in
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
    | Tuple (es) -> Tuple (List.map f es)
    | Vector (es) -> Vector (List.map f es)
    | If (e1, e2, e3) -> If (f e1, f e2, f e3)
    | Let ((v), e1, Variable (v')) when v = v' -> e1
    | Let ((v), Variable (v'), e2) -> substitute_varname [v, v'] e2
    | Let ((v), e1, e2) -> Let ((v), f e1, f' v e2)
    | LetFun ((v), args, LetFun ((v'), args', e1', Variable (v'')), e2) when v' = v'' ->
      LetFun ((v), args @ args', f''' v v' args e1', f e2)
    | LetFun ((v), args, e1, e2) ->
      LetFun ((v), args, f'' args e1, f'' args e2)
    | Match _ -> undefined ()

let fuse l =
  let rec f n l =
    let l' = fuse' [] l in
    if n < 0 || l = l'
    then l
    else f (n - 1) l'
  in
  f max_iter l
    



let rec lift r =
  let defs = ref [] in
  let rec f r = match r with
    | External _ -> r
    | Constant _ -> r
    | Variable _ -> r
    | LetFun ((v), args, e1, e2) when freevars r = [] ->
      let bn = gen_varname () in
      defs := {fun_name = (bn); args = args; body = e1} :: !defs;
      f (substitute_varname [v, bn] e2)
    | LetFun ((v), args, e1, e2) -> invalid_arg "lift"
    | Apply (e, es) -> Apply (f e, List.map f es)
    | Tuple (es) -> Tuple (List.map f es)
    | Vector (es) -> Vector (List.map f es)
    | If (e1, e2, e3) ->  If (f e1, f e2, f e3)
    | Let ((v), e1, e2) -> Let ((v), f e1, f e2)
    | Match _ -> undefined ()
  in
  let r' = f r in
  r', defs

let vt_to_sexprs = function v, t -> [Sexpr.Sident v; T.oType_to_sexpr t]
let vt_to_sexpr = function v, t -> Sexpr.Sexpr (vt_to_sexprs (v, t))
let lt_to_sexprs = function Id.L v, t -> [Sexpr.Sident v; T.oType_to_sexpr t]
let lt_to_sexpr = function v, t -> Sexpr.Sexpr (lt_to_sexprs (v, t))

let rec to_sexpr = function
  | Constant (lit) -> Sexpr.tagged_sexpr "l:const" [Syntax.lit_to_sexpr lit]
  | External (v, t) -> Sexpr.tagged_sexpr "l:external" (vt_to_sexprs (v, t))
  | Variable (v) -> Sexpr.tagged_sexpr "l:var" [Sexpr.ident v]
  | Apply (e, es) -> Sexpr.tagged_sexpr "l:apply" (List.map to_sexpr (e :: es))
  | Tuple (es) -> Sexpr.tagged_sexpr "l:tuple" (List.map to_sexpr es)
  | Vector (es) -> Sexpr.tagged_sexpr "l:vector" (List.map to_sexpr es)
  | If (e1, e2, e3) -> Sexpr.tagged_sexpr "l:if" (List.map to_sexpr [e1; e2; e3])
  | Let ((v), e1, e2) -> Sexpr.tagged_sexpr "l:let" [Sexpr.tagged_sexpr v [to_sexpr e1]; to_sexpr e2]
  | LetFun ((v), args, e1, e2) -> Sexpr.tagged_sexpr "l:letfun" [Sexpr.Sexpr (Sexpr.Sident v :: List.map Sexpr.ident args); Sexpr.Sexpr (List.map to_sexpr [e1; e2])]
  | Match _ -> undefined ()

let decl_to_sexpr = function
  | {fun_name = vt; args = args; body = e} ->
      Sexpr.tagged_sexpr "fun-decl" [
        Sexpr.tagged_sexpr "name" [Sexpr.Sident vt];
        Sexpr.tagged_sexpr "args" (List.map Sexpr.ident args);
        Sexpr.tagged_sexpr "body" [to_sexpr e];
        ]

let decls_to_sexpr decls =
  Sexpr.tagged_sexpr "l:decls" (List.map decl_to_sexpr decls)

let to_decl name = function
  | LetFun ((v), args, e, Variable v') when v = v' -> 
    {fun_name = (name); args = args; body = substitute_varname [v, name] e}
  | LetFun _ as r-> failwith (Printf.sprintf "Unexpected argument. Maybe, lambda lifting is mistaken. Got: %s" (Sexpr.to_string (to_sexpr r)))
  | e ->
    {fun_name = (name); args = []; body = e}

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
    | Variable (v') -> 
      substitute_decl_name [v', v] !defs
    | _ -> 
      to_decl v l'''  :: !defs

(*
let from_module m =
  let defs = Module.defs_expr_cont m in
  let vrs = List.map (function v, (qtvs, ts, r) -> v, r) defs in
  let vls = List.map (function v, r -> v, of_typint_result r) vrs in
  let vs = List.map fst vls in
  List.concat (List.map (function v, l -> convert vs v l) vls)
*)