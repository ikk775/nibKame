
type id_or_imm = V of Id.t | C of int
type cmp_op =
  | Eq | NotEq | LsEq | Ls | Gt | GtEq

type literal =
  | Int_l of int | Char_l of char | Pointer_l of Id.l

type ty =
  | Int | Float | Char | Pointer of Type.t
  | Fun

type mem_op =
  | Direct of Id.t
  | Label of Id.l
  | Plus_offset of Id.t * id_or_imm
  | Scaled_offset of Id.t * Id.t * int

let to_ty = function
  | Type.Int -> Int
  | Type.Float -> Float
  | Type.Char -> Char
  | Type.Fun _ -> Fun
  | _ as t -> Pointer t

type t =
  | Ans of exp
  | Seq of t * t
  | Let of (Id.t * ty) * exp * t
and exp =
  | Nop
  | Set of literal
  | Mov of id_or_imm
  | Neg of Id.t
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Mul of Id.t * id_or_imm (* imul *)
  | Div of Id.t * id_or_imm (* cdq and idiv *)
  | SLL of Id.t * id_or_imm
  | SLR of Id.t * id_or_imm
  | Ld of mem_op (* load to *)
  | St of Id.t * mem_op (* store *)
  | FMov of Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | FLd of mem_op
  | FSt of Id.t * mem_op

  | BLd of mem_op
  | BSt of Id.t * mem_op

  | Comp of cmp_op * ty * Id.t * id_or_imm
  | If of t * t * t

  | ApplyCls of Id.t * Id.t list
  | ApplyDir of Id.l * Id.t list

  | ArrayRef of Id.t * Id.t
  | ArraySet of Id.t * Id.t * Id.t

  | Cons of Id.t * Id.t
  | Car of Id.t
  | Cdr of Id.t  | FCons of Id.t * Id.t
  | FCar of Id.t
  | FCdr of Id.t

  | TupleAlloc of (Id.t * ty) list
  | ArrayAlloc of ty * Id.t

  | Save of Id.t * Id.t
  | Pop of Id.t * Id.t

type fundef = { name: Id.l; args: (Id.t * ty) list; body: t; ret: ty }

let float_literal_list : (float * Id.l) list ref = ref []

let tuple_size types =
  List.fold_left
    (fun s -> function 
       | Float -> s + 8
       | _ -> s + 4)
    4
    types

let array_size len = function
  | Float -> 4 + 8 * len
  | Char -> 4 + 1 * len
  | _ -> 4 + 4 * len

module M = 
  struct
    include Id.Map
    let add_twin (key, x) map =
      add key x map
  end

let counter = ref 0
let genid () =
  incr counter;
  Printf.sprintf "fv%d" !counter
let counter1 = ref 0
let temp () =
  incr counter1;
  Printf.sprintf "tmp%d" !counter1

let add_list xys env = List.fold_left (fun env (x, y) -> M.add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> M.add x y env) env xs ys

let add_float_table f =
  try
    snd (List.find (fun (f',_) -> f = f') !float_literal_list)
  with Not_found ->
    let label = temp () in float_literal_list := (f, Id.L(label)) :: !float_literal_list;
      Id.L(label)

let comp_cmp_op = function
  | Closure.Eq -> Eq
  | Closure.NotEq -> NotEq
  | Closure.Ls -> Ls
  | Closure.LsEq -> LsEq
  | Closure.Gt -> Gt
  | Closure.GtEq -> GtEq

(*
  e1 の値を e1 の最後で束縛し, e2 を後ろへ
*)
let rec let_concat e1 var e2 =
  match e1 with
    | Ans i -> Let (var, i, e2)
    | Seq (t1, t2) -> Seq (t1, let_concat t2 var e2)
    | Let (v, e, t) ->
	Let (v, e, let_concat t var e2)

let var_with_type env (x : Id.t) =
  x, M.find x env

let to_ty_with_var (v, t)  =
  v, to_ty t

let rec compile_exp env = function
  | Closure.Unit -> Ans(Nop)
  | Closure.Int i -> Ans(Set(Int_l i))
  | Closure.Char c -> Ans(Set(Char_l c))
  | Closure.Float f -> let l = add_float_table f in Ans(Set(Pointer_l l))
  | Closure.Seq (t1, t2) -> Seq (compile_exp env t1, compile_exp env t2)
  | Closure.Neg l -> Ans(Neg(l))
  | Closure.Add (a, b) -> Ans(Add(a, V b))
  | Closure.Sub (a, b) -> Ans(Sub(a, V b))
  | Closure.Mul (a, b) -> Ans(Mul(a, V b))
  | Closure.Div (a, b) -> Ans(Div(a, V b))
  | Closure.FAdd (a, b) -> Ans(FAdd(a, b))
  | Closure.FSub (a, b) -> Ans(FSub(a, b))
  | Closure.FMul (a, b) -> Ans(FMul(a, b))
  | Closure.FDiv (a, b) -> Ans(FDiv(a, b))
  
  | Closure.FNeg a -> Ans(FNeg a)

  | Closure.If (cp, a, b, t, f) ->
      let compare = Ans (Comp (comp_cmp_op cp, M.find a env, a, V b)) in
	Ans (If (compare, compile_exp env t, compile_exp env f))
  | Closure.Let ((var, t), e1, e2) ->
      let e1' = compile_exp env e1 in
      let e2' = compile_exp (M.add var (to_ty t) env) e2 in
	let_concat e1' (var, (to_ty t)) e2'

  | Closure.Var x ->
      begin match M.find x env with
	| Float -> Ans(FMov x)
	| _ -> Ans(Mov (V x))
      end
  | Closure.MakeCls (dst, {Closure.entry = label; Closure.actual_fv = fv}, e) -> 
      let t = temp () in 
      let tuple = TupleAlloc((t, Int) :: List.map (var_with_type env) fv) in
	Let ((t, Int), Set (Pointer_l label),
	     Ans tuple)
  | Closure.ApplyCls (cls, vars) -> Ans (ApplyCls (cls, vars))
  | Closure.ApplyDir (label, vars) -> Ans (ApplyDir (label, vars))
  | Closure.Tuple vars -> Ans(TupleAlloc(List.map (var_with_type env) vars))
  | Closure.LetTuple (dsts, tuple, e) ->
      let tuple_store list var dst e =
	let rec iter = function 
	  | (v, t) :: tl when v = var -> []
	  | (v, t) :: tl -> t :: iter tl
	  | [] -> failwith (Format.sprintf "not found in tuple, %s" var)
	in
	let offset = (tuple_size (iter list)) - 4 in
	  Let (dst, Ld (Plus_offset(tuple, C offset)), e)
      in
      let dsts' = List.map to_ty_with_var dsts in
      let env' = add_list dsts' env in
      let e' = compile_exp env' e in
	List.fold_right (fun c e -> tuple_store dsts' (fst c) c e) dsts' e'

  | Closure.Ref t -> Ans (Ld (Direct t))
  | Closure.Set (t, u) -> 
      begin match M.find t env with 
	| Float -> Ans (FSt (t, Direct u))
	| _ -> Ans (St (t, Direct u))
      end

  | Closure.ArrayAlloc (typ, num) -> Ans (ArrayAlloc (to_ty typ, num))
  | Closure.ArrayRef (ary, num) ->
      begin match M.find ary env with
	| Pointer (Type.Array typ) ->
	    begin match typ with
	      | Type.Float -> Ans (FLd (Scaled_offset (ary, num, 8)))
	      | Type.Char -> Ans (BLd (Plus_offset (ary, V num)))
	      | _ -> Ans (Ld (Scaled_offset (ary, num, 4)))
	    end
	| _ -> failwith (Format.sprintf "%s is not an array." ary)
      end
  | Closure.ArraySet (ary, num, data) ->
      begin match M.find data env with
	| Float -> Ans (FSt (data, Scaled_offset (ary, num, 8)))
	| Char -> Ans (BSt (data, Plus_offset (ary, V num)))
	| _ -> Ans (St (data, Scaled_offset (ary, num, 4)))
      end

  | Closure.Cons (hd, tl) -> Ans (Cons (hd, tl))
  | Closure.Car t -> Ans (Car t)
  | Closure.Cdr t -> Ans (Cdr t)
  | Closure.FCons (hd, tl) -> Ans (FCons (hd, tl))
  | Closure.FCar t -> Ans (FCar t)
  | Closure.FCdr t -> Ans (FCdr t)

  | Closure.ExtArray l -> Ans (Set (Pointer_l l))

(*
  自由変数は関数へのポインタと一緒にしたタプルとして渡される 
  現状ではたとえ自由変数がなくとも引数にタプルが渡される
*)
let compile_fun { Closure.fun_name = (Id.L(label), t);
		  Closure.args = args; Closure.formal_fv = free_vars;
		  Closure.body = exp} env =
  let env = M.add label (to_ty t) (add_list (List.map to_ty_with_var args) (add_list (List.map to_ty_with_var free_vars) env)) in
  let Type.Fun (_,t2) = t in
    match List.map snd free_vars with
      | [] -> 
	  let e = compile_exp env exp in
	    { name = Id.L(label); args = (genid (), Pointer (Type.Tuple [Type.Unit])) :: (List.map to_ty_with_var args); body = e; ret = to_ty t2 }
      | fvs ->
	  let fv = (genid (), Pointer (Type.Tuple (Type.Unit :: fvs))) in
	  let e = compile_exp (M.add (fst fv) (snd fv) env) exp in
	    { name = Id.L(label); args = fv :: (List.map to_ty_with_var args); body = e; ret = to_ty t2 }

let fundefs : fundef list ref = ref []
let main = ref (Ans(Set (Int_l 0)))

let var_to_exp { Closure.var_name = (Id.L label, typ); Closure.expr = expr } env =
  let tmp = temp () in
  let typ' = to_ty typ in
  let env' = M.add label typ' env in
    main := let_concat
      (compile_exp env expr)
      (tmp, typ')
      (Seq (Ans(match typ' with
		 | Float -> FSt(tmp, Label (Id.L label))
		 | Char -> BSt(tmp, Label (Id.L label))
		 | _ -> St(tmp, Label (Id.L label))), !main));
    env'

(*
  f : Closure.topDecl list ->
        -> fundef list * (float * Id.l) list
  戻り値は後ろから順に処理されていって欲しい. 具体的に言うとList.rev_iterとかで
*)
let f declears =
  let iter declear env =
    match declear with
      | Closure.VarDecl var ->
	  var_to_exp var env
      | Closure.FunDecl func ->
	  fundefs := compile_fun func env :: !fundefs;
	  env
  in
  let e = List.fold_right iter declears M.empty in
  let start = { name = Id.L("nibkame_entry"); args = []; body = !main; ret = Int } in
    (start :: !fundefs), !float_literal_list
