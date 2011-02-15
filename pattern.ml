open MyUtil

module R = Typing
module T = TypingType
module E = TypingExpr

type value =
  | TP_Constant of Syntax.lit * TypingType.oType
  | TP_Variable  of Id.t * TypingType.oType
  | TP_Constructor of Id.t * TypingType.oType

type branch =
  | TP_Value of value * tree
  | TP_Apply of tree
  | TP_Tuple of tree
  | TP_Vector of tree
(* The most general pattern is the last. *)
and tree =
  | TP_Node of branch list
  | TP_Leaf of R.result option * R.result

type t = R.result * tree

type coconstructor =
  | VariantId of int * T.oType
  | Function of (T.oType * Id.t) list

type coconstructors = Id.t (*constructor name*) * coconstructor

let gen_var_num = ref 0

let gen_varname t =
  gen_var_num := !gen_var_num + 1;
  Format.sprintf "$p:%d" !gen_var_num

let rec map_tree_leaf f = function
  | TP_Leaf (None, expr) -> TP_Leaf (None, f expr)
  | TP_Leaf (Some guard, expr) -> TP_Leaf (Some (f guard), f expr)
  | TP_Node brs -> TP_Node (List.map (map_branch_leaf f) brs)
and map_branch_leaf f = function
  | TP_Value (value, tree) -> TP_Value (value, map_tree_leaf f tree)
  | TP_Apply trees -> TP_Apply (map_tree_leaf f trees)
  | TP_Tuple trees -> TP_Tuple (map_tree_leaf f trees)
  | TP_Vector trees -> TP_Vector (map_tree_leaf f trees)

let subst_tree_expr_name ss tree =
  map_tree_leaf (R.substitute_varname ss) tree


let unfold_do f venv cls =
  List.concat (List.map (f venv) cls)

let unfold_do f venv cls =
  List.concat (List.map (f venv) cls)
  
let unfold_or venv clause =
  let pat, guard, expr = clause in
  let rec f pat = match pat with
    | R.RP_Constant _ | R.RP_Variable _ | R.RP_Constructor _ -> [pat]
    | R.RP_Apply ((p1, p2), t) -> List.map (function [p1; p2] -> R.RP_Apply ((p1, p2), t) | _ -> failwith "something went wrong.") (MyUtil.List.select [f p1; f p2])
    | R.RP_And ((p1, p2), t)-> List.map (function [p1; p2] -> R.RP_And ((p1, p2), t) | _ -> failwith "something went wrong.") (MyUtil.List.select [f p1; f p2])
    | R.RP_Tuple (ps, t) -> List.map (function ps -> R.RP_Tuple (ps, t)) (MyUtil.List.select (List.map f ps))
    | R.RP_Vector (ps, t) -> List.map (function ps -> R.RP_Vector (ps, t)) (MyUtil.List.select (List.map f ps))
    | R.RP_Not (p, t)-> List.map (function p -> R.RP_Not (p, t)) (f p)
    | R.RP_Or ((p1, p2), t) -> f p1 @ f p2
  in
  List.map (fun p -> p, guard, expr) (f pat)

let rec unify_and ss p1 p2 =
  let f ss ps' =
    let rec f' ss ps = function
      | [] -> ps, ss
      | (p1', p2') :: ps' ->
        let p'', ss' = unify_and ss p1' p2' in
        f' ss' (p'' :: ps) ps'
    in
    let ps'', ss = f' ss [] ps' in
    List.rev ps'', ss in
  let g ss p11 p12 p21 p22 =
    let p1', ss = unify_and ss p11 p12 in
    let p2', ss = unify_and ss p21 p22 in
    p1', p2', ss in
  match p1, p2 with
    | (R.RP_Variable _ as v), p
    | p, (R.RP_Variable _ as v) -> p, ((v, p) :: ss)
    | R.RP_And ((p1, p2), t), p
    | p, R.RP_And ((p1, p2), t) ->
      let p', ss = unify_and ss p1 p2 in
      unify_and ss p p'
    | R.RP_Apply ((pf1, pa1), t1), R.RP_Apply ((pf2, pa2), t2) ->
      let pf', pa', ss = g ss pf1 pf2 pa1 pa2 in
      R.RP_Apply ((pf', pa'), t1), ss
    | R.RP_Tuple (ps1, t1), R.RP_Tuple (ps2, t2) -> 
      let ps', ss' = f ss (List.combine ps1 ps2) in
      R.RP_Tuple(ps', t1), ss
    | R.RP_Vector (ps1, t1), R.RP_Vector (ps2, t2) -> 
      let ps', ss' = f ss (List.combine ps1 ps2) in
      R.RP_Vector(ps', t1), ss
    | R.RP_Not _, p
    | p, R.RP_Not _ -> failwith "negative pattern is not supported yet."
(*i
    | R.RP_Tuple (ps, t) -> List.map (function ps -> R.RP_Tuple (ps, t)) (MyUtil.List.select (List.map f ps))
    | R.RP_Vector (ps, t) -> List.map (function ps -> R.RP_Vector (ps, t)) (MyUtil.List.select (List.map f ps))
let unfold_and venv clause =
  let pat, guard, expr = clause in
  let rec f pat = match pat with
    | R.RP_And ((p1, p2), t)-> 
    | R.RP_Constant _ | R.RP_Variable _ | R.RP_Constructor _ -> pat, []
    | R.RP_Apply ((p1, p2), t) -> R.RP_Apply(
    | R.RP_Tuple (ps, t) -> 
    | R.RP_Vector (ps, t) -> 
    | R.RP_Not (p, t)-> 
    | R.RP_Or ((p1, p2), t) -> 
  in
  (undefined ())
i*)
(*i
let unfold_pat_subst_constructor venv cls =
  let pat, guard, expr = cls in
  let rec f = function
    | R.RP_Constant _ | R.RP_Variable _ -> pat, []
    | R.RP_Apply ((R.RP_Constructor (id, tc) , p2), ta)-> 
      let p2', bs = f p2 in
      match List.assoc id venv with
        | VariantId (i, et) -> 
          let si = Syntax.Int i in
          let ti = T.O_Constant Type.Int in
          R.RP_Tuple ([R.RP_Constant (si, ti); p2'], ta), bs
        | Function txs -> 
          let t', fname = List.find (function t, v -> T.unifiable tc t) txs in
          let tcf = T.arg_type tc in
          let tct = T.dest_type tc in
          let b = R.gen_var tcf in
          let bn = R.varname b in
(*i          R.RP_Variable (bn, tcf), [bn, ]i*)
          (undefined ())
(*i
    | RP_And of (pattern * pattern) * TypingType.oType
    | RP_Or of (pattern * pattern) * TypingType.oType (* Both patterns must have a same set of variables. And each variable has same type across the patterns. *)
    | RP_Not of pattern * TypingType.oType
    | RP_Tuple of pattern list * TypingType.oType
    | RP_Vector of pattern list * TypingType.oType
  in
  f pat
i*) i*)

let is_variable = function TP_Value (TP_Variable _, _) -> true | _ -> false

let varname = function
  | TP_Value (TP_Variable (v, t), _) -> v
  | _ -> failwith "something went wrong."

(*i
let rec add_branch_rev rbr = function (* reverse-ordered and priority-ordered branch -> normal order branch -> reverse-ordered and priority-ordered branch*)
  | [] -> rbr
  | TP_Leaf (None, expr) as tr :: trs -> tr :: rbr
  | TP_Leaf (Some guard, expr) as tr :: trs -> normalize_branch trs @ tr :: rbr
  | TP_Value (TP_Variable (v', t'), trcs') as trv :: trs ->
    begin match rbr with
      | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
      | rbr -> add_branch_rev (trv :: rbr) trs
    end
  | TP_Value (TP_Variable (v', t'), trcs') as trv :: trs ->
    begin match rbr with
      | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
      | rbr -> add_branch_rev (trv :: rbr) trs
    end
  | TP_Value (TP_Constructor (v', t') as c', trcs') as tr' :: trs' ->
    let cs, rtrs = List.partition (function TP_Value (c, trcs) when c = c' -> true | _ -> false) rbr
    begin match cs with
      | [] -> 
      | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
      | rbr -> add_branch_rev (trv :: rbr) trs
    end
    i*)
    (*i
let rec add_branch_rev rbr br' = match rbr with (* reverse-ordered and priority-ordered branch -> normal order branch -> reverse-ordered and priority-ordered branch*)
  | [] -> List.rev rbr
  | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
    match br' with
  | TP_Leaf (None, expr) as tr :: trs -> tr :: rbr
  | TP_Leaf (Some guard, expr) as tr :: trs -> normalize_branch trs @ tr :: rbr
  | TP_Value (TP_Variable (v', t'), trcs') as trv :: trs ->
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
  | TP_Value (TP_Variable (v', t'), trcs') as trv :: trs ->
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
  | TP_Value (TP_Constructor (v', t') as c', trcs') as tr' :: trs' ->
    let cs, rtrs = List.partition (function TP_Value (c, trcs) when c = c' -> true | _ -> false) rbr
    begin match cs with
      | [] -> 
      | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
      | rbr -> add_branch_rev (trv :: rbr) trs
    end
  | TP_Leaf (None, expr) as tr :: trs -> tr :: rbr
  | TP_Leaf (Some guard, expr) as tr :: trs -> normalize_branch trs @ tr :: rbr
  | TP_Value (TP_Variable (v', t'), trcs') as trv :: trs ->
    begin match rbr with
      | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
      | rbr -> add_branch_rev (trv :: rbr) trs
    end
  | TP_Value (TP_Variable (v', t'), trcs') as trv :: trs ->
    begin match rbr with
      | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
      | rbr -> add_branch_rev (trv :: rbr) trs
    end
  | TP_Value (TP_Constructor (v', t') as c', trcs') as tr' :: trs' ->
    let cs, rtrs = List.partition (function TP_Value (c, trcs) when c = c' -> true | _ -> false) rbr
    begin match cs with
      | [] -> 
      | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
      | rbr -> add_branch_rev (trv :: rbr) trs
    end
    i*)
    (*i
and normalize_branch = function
  | [] -> []
  | [tr] -> [tr]
  | TP_Leaf (None, expr) as tr :: trs -> [tr]
  | TP_Value (TP_Variable _, trcs) as v :: [] -> [v]
  | TP_Value (TP_Variable _, trcs) as v :: trs ->
    let vs, trs' = List.partition is_variable trs in
    normalize_branch (trs' @ add_branch_rev [v] vs)
   i*) 
(*i  | TP_Apply trcs :: trs, TP_Apply trcs' :: trs'-> add_tree (TP_Apply (add_tree trcs trcs') :: trs) trs'
  | TP_Nand trcs :: trs, TP_Nand trcs' :: trs'-> add_tree (TP_Nand (add_tree trcs trcs') :: trs) trs'
  | TP_Tuple trcs :: trs, TP_Tuple trcs' :: trs'-> add_tree (TP_Tuple (add_tree trcs trcs') :: trs) trs'
  | TP_Vector trcs :: trs, TP_Vector trcs' :: trs'-> add_tree (TP_Vector (add_tree trcs trcs') :: trs) trs'
 i*) 

(*i
let rec add_branch_rev rbr rbr' = match rbr with (* reverse-ordered and priority-ordered branch -> normal order branch -> reverse-ordered and priority-ordered branch*)
  | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
    begin match rbr with
      | TP_Value (TP_Variable (v', t'), trcs') as trv :: trs ->
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
      | TP_Value (TP_Variable (v', t'), trcs') as trv :: trs ->
        add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
      | TP_Value (TP_Constructor (v', t') as c', trcs') as tr' :: trs' ->
        let cs, rtrs = List.partition (function TP_Value (c, trcs) when c = c' -> true | _ -> false) rbr in
        begin match cs with
          | TP_Value (TP_Variable (v, t), trcs) :: rbr' -> 
            add_branch_rev (TP_Value (TP_Variable (v, t), add_branch_rev trcs (List.map (subst_expr_name [v', v]) trcs')) :: rbr') trs
          | rbr -> add_branch_rev (trv :: rbr) trs
        end
    end
i*)
let rec branch_type = function
  | TP_Value (TP_Constant (_, t), _)
  | TP_Value (TP_Variable (_, t), _)
  | TP_Value (TP_Constructor (_, t), _) -> t
  | TP_Apply (TP_Node brs) -> T.dest_type (branches_type brs)
  | TP_Tuple (TP_Node brs) -> T.O_Tuple (List.map branch_type brs)
  | TP_Vector (TP_Node brs) -> T.O_Vector (branches_type brs)
  | TP_Apply (TP_Leaf _)
  | TP_Tuple (TP_Leaf _)
  | TP_Vector (TP_Leaf _) -> failwith "something went wrong."
and branches_type = function
  | [] -> failwith "something went wrong."
  | br :: brs -> branch_type br

let rec implies_branch br1 br2 =
  match br1 with
    | TP_Value (TP_Variable _, TP_Leaf _) -> true
    | TP_Value (TP_Variable _, tr1) ->
      begin match br2 with
        | TP_Value (TP_Variable _, TP_Leaf _) -> false
        | TP_Value (TP_Variable _, tr2)
        | TP_Value (TP_Constant _, tr2) 
        | TP_Value (TP_Constructor _, tr2) 
        | TP_Apply tr2
        | TP_Tuple tr2
        | TP_Vector tr2 -> implies_tree tr1 tr2
      end
    | TP_Value (TP_Constant _ as c1, tr1) -> 
      begin match br2 with
        | TP_Value (TP_Variable _, _) -> false
        | TP_Value (TP_Constant _ as c2, tr2) when c1 = c2 -> implies_tree tr1 tr2
        | _ -> false
      end
    | TP_Value (TP_Constructor _ as c1, tr1) -> 
      begin match br2 with
        | TP_Value (TP_Variable _, _) -> false
        | TP_Value (TP_Constructor _ as c2, tr2) when c1 = c2 -> implies_tree tr1 tr2
        | _ -> false
      end
    | TP_Apply tr1 -> 
      begin match br2 with
        | TP_Value (TP_Variable _, _) -> false
        | TP_Apply tr2 -> implies_tree tr1 tr2
        | _ -> false
      end
    | TP_Tuple tr1 -> 
      begin match br2 with
        | TP_Value (TP_Variable _, _) -> false
        | TP_Tuple tr2 -> implies_tree tr1 tr2
        | _ -> false
      end
    | TP_Vector tr1 -> 
      begin match br2 with
        | TP_Value (TP_Variable _, _) -> false
        | TP_Vector tr2 -> implies_tree tr1 tr2
        | _ -> false
      end
and implies_tree tr1 tr2 = match tr1, tr2 with
  | TP_Leaf _, TP_Leaf _ -> true
  | TP_Node brs1, TP_Node brs2 ->
    List.for_all (fun br2 -> List.exists (fun br1 -> implies_branch br1 br2) brs1) brs2
  | _ -> invalid_arg "implies_tree"

let is_unificatable_branch br1 br2 = match br1, br2 with
  | TP_Value (TP_Variable _, _), _ -> true
  | _, TP_Value (TP_Variable _, _) -> true
  | TP_Value (TP_Constant _ as c1, _), TP_Value (TP_Constant _ as c2, _) when c1 = c2 -> true
  | TP_Value (TP_Constructor _ as c1, _), TP_Value (TP_Constructor _ as c2, _) when c1 = c2 -> true
  | TP_Apply _, TP_Apply _ -> true
  | TP_Tuple _, TP_Tuple _ -> true
  | TP_Vector _, TP_Vector _ -> true
  | TP_Value (TP_Constant _, _), _
  | TP_Value (TP_Constructor _, _), _
  | TP_Apply _, _
  | TP_Tuple _, _
  | TP_Vector _, _ -> false
and is_unificatable_tree tr1 tr2 = match tr1, tr2 with
  | TP_Leaf _, TP_Leaf _ -> true
  | TP_Node brs1, TP_Node brs2 ->
    List.for_all (fun br2 -> List.exists (fun br1 -> implies_branch br1 br2) brs1) brs2
  | _ -> invalid_arg "implies_tree"

let is_similar_branch br1 br2 = match br1, br2 with
  | TP_Value (TP_Constant _ as c1, _), TP_Value (TP_Constant _ as c2, _) when c1 = c2 -> true
  | TP_Value (TP_Constructor _ as c1, _), TP_Value (TP_Constructor _ as c2, _) when c1 = c2 -> true
  | TP_Value (TP_Variable _, _), TP_Value (TP_Variable _, _) -> true
  | TP_Apply _, TP_Apply _ -> true
  | TP_Tuple _, TP_Tuple _ -> true
  | TP_Vector _, TP_Vector _ -> true
  | TP_Value (TP_Constant _, _), _
  | TP_Value (TP_Constructor _, _), _
  | TP_Value (TP_Variable _, _), _
  | TP_Apply _, _
  | TP_Tuple _, _
  | TP_Vector _, _ -> false

let rec add_exception brs tr' =
  let vs, brs' = List.partition is_variable brs in
  match tr' with
   | TP_Node brs -> undefined ()
   | TP_Leaf (guard, leaf) ->
    begin match vs with
      | [] -> 
        let t = branches_type brs in
        let bn = gen_varname t in
        brs @ [TP_Value (TP_Variable (bn, t), tr')]
      | [TP_Value (v, tr'')] -> 
        brs' @ [TP_Value (v, merge_tree tr'' tr')]
      | v1 :: vs' -> 
        (*i
        match List.fold_left merge_branch v1 vs' with
          | TP_Value (v, tr'') -> brs' @ [TP_Value (v, merge_tree tr'' tr')]
          | _ -> failwith "something went wrong"
        i*)
        undefined ()
    end
and add_branch brs br' =
  let vs, brs' = List.partition is_variable brs in
  let f br =
    if is_similar_branch br br'
    then merge_branch br br'
    else br in
  match vs with
    | [] ->
      if List.exists (fun br -> is_similar_branch br br') brs
      then List.map f brs
      else brs @ [br']
    | [v] ->
      if implies_branch v br'
      then brs' @ [merge_branch v br']
      else if List.exists (fun br -> is_similar_branch br br') brs
      then List.map f brs
      else brs' @ br' :: [v]
    | _ -> (undefined ())
and merge_branch br br' = match br, br' with
  | TP_Value (TP_Variable _ as c1, tr1), TP_Value (TP_Constructor _ as c2, tr2) -> invalid_arg "merge_branch"
  | TP_Value (TP_Constant _ as c1, tr1), TP_Value (TP_Constant _ as c2, tr2) when c1 = c2 -> TP_Value (c1, merge_tree tr1 tr2)
  | TP_Value (TP_Constant _ as c1, tr1), TP_Value (TP_Constant _ as c2, tr2) -> invalid_arg "merge_branch"
  | TP_Value (TP_Constructor _ as c1, tr1), TP_Value (TP_Constructor _ as c2, tr2) when c1 = c2 -> TP_Value (c1, merge_tree tr1 tr2)
  | TP_Value (TP_Constructor _ as c1, tr1), TP_Value (TP_Constructor _ as c2, tr2) -> invalid_arg "merge_branch"
and merge_branches = function
  | [] -> invalid_arg "merge_branches"
  | [br] -> br
  | br :: brs -> 
    List.fold_left merge_branch br brs
and merge_tree tr tr' = match tr, tr' with
  | TP_Node brs, TP_Node brs' -> TP_Node (List.fold_left add_branch brs brs')
  | TP_Node brs, TP_Leaf (guard, leaf) -> TP_Node (add_exception brs tr')
  | TP_Leaf (guard, leaf), _ -> tr

let add_trees = function
  | [] -> TP_Node []
  | [tr] -> tr
  | tr :: trs -> List.fold_left merge_tree tr trs

let rec tree_of_clause = function pat, guard, clause ->
  let rec f tree = function
    | R.RP_Variable (None, t) ->
      let bn = gen_varname t in
      TP_Node [TP_Value (TP_Variable (bn, t), tree)]
    | R.RP_Variable (Some v, t) ->
      let bn = gen_varname t in
      let ss = [v, bn] in
      TP_Node [TP_Value (TP_Variable (bn, t), subst_tree_expr_name ss tree)]
    | R.RP_Constant (v, t) -> TP_Node [TP_Value (TP_Constant (v, t), tree)]
    | R.RP_Constructor (v, t) -> TP_Node [TP_Value (TP_Constructor (v, t), tree)]
    | R.RP_Apply ((p1, p2), t) -> TP_Node [TP_Apply (add_trees [List.fold_left f tree (List.rev [p1; p2])])]
    | R.RP_Tuple (ps, t) -> TP_Node [TP_Tuple (add_trees [List.fold_left f tree (List.rev ps)])]
    | R.RP_Vector (ps, t) -> TP_Node [TP_Vector (add_trees [List.fold_left f tree (List.rev ps)])]
    | R.RP_And ((p1, p2), t)-> failwith "conjunctive pattern is not supported yet."
    | R.RP_Or ((p1, p2), t)-> failwith "disjunctive pattern is not supported yet."
    | R.RP_Not (p, t) -> failwith "negative pattern is not supported yet."
  in
  f (TP_Leaf (guard, clause)) pat

let unfold venv = function pat, guard, expr -> 
  (undefined ())

let unfold_result venv r =
  (undefined ())

let unfold_module m =
  m
  
let is_rp_variable = function
  | R.RP_Variable _ -> true
  | _ -> false

let is_tuple_normal = function
  | R.RP_Tuple (ps, t) -> List.for_all is_rp_variable ps
  | _ -> false

let is_pattern_normal pat = undefined ()
