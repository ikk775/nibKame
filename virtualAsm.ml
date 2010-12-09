
type id_or_imm = V of Id.t | C of int
type cmp_op =
  | Eq | NotEq | LsEq | Ls | Gt | GtEq

type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp =
  | Nop
  | SetL of Id.l (* ラベルにストア *)
  | Mov of id_or_imm
  | Neg of Id.t
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Mul of Id.t * id_or_imm (* imul *)
  | Div of Id.t * id_or_imm (* cdq and idiv *)
  | SLL of Id.t * id_or_imm
  | SLR of Id.t * id_or_imm
  | Ld of Id.t * id_or_imm
  | St of Id.t * Id.t * id_or_imm
  | FMov of Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | FLd of Id.t * id_or_imm
  | FSt of Id.t * Id.t * id_or_imm

  | Comp of cmp_op * Type.t * Id.t * id_or_imm
  | If of t * t * t

  | ApplyCls of Id.t * Id.t list * Id.t list
  | ApplyDir of Id.l * Id.t list * Id.t list

  | ArrayRef of Id.t * Id.t
  | ArraySet of Id.t * Id.t * Id.t

  | Cons of Id.t * Id.t
  | Car of Id.t
  | Cdr of Id.t
  | FCons of Id.t * Id.t
  | FCar of Id.t
  | FCdr of Id.t

  | Save of Id.t * Id.t
  | Pop of Id.t * Id.t

type fundef = { name: Id.l; args: (Id.t * Type.t) list; body: t; ret: Type.t }

let float_litearl_list : (float * Id.l) list ref = ref []

let tuple_size types =
  List.fold_left
    (fun s -> function 
       | Type.Float -> s + 8
       | _ -> s + 4)
    4
    types

let array_size len = function
  | Type.Float -> 4 + 8 * len
  | Type.Char -> 4 + 1 * len
  | _ -> 4 + 4 * len

module M = Id.Map

let counter = ref 0
let genid () =
  incr counter;
  Printf.sprintf "fv%d" !counter

let add_list xys env = List.fold_left (fun env (x, y) -> M.add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> M.add x y env) env xs ys

let rec compile_exp env = function
  | _ -> (MyUtil.undefined ())

(*
  自由変数は関数へのポインタと一緒にしたタプルとして渡される 
  現状ではたとえ自由変数がなくとも引数にタプルが渡される
*)
let compile_fun { Closure.fun_name = (Id.L(label), t);
		  Closure.args = args; Closure.formal_fv = free_vars;
		  Closure.body = exp} =
  let env = M.add label t (add_list args (add_list free_vars M.empty)) in
  let Type.Fun (_,t2) = t in
    match List.map snd free_vars with
      | [] -> 
	  let e = compile_exp env exp in
	    { name = Id.L(label); args = (genid (),(Type.Tuple [Type.Unit])) :: args; body = e; ret = t2 }
      | fvs ->
	  let fv = (genid (), Type.Tuple (Type.Unit :: fvs)) in
	  let e = compile_exp (M.add (fst fv) (snd fv) env) exp in
	    { name = Id.L(label); args = fv :: args; body = e; ret = t2 }
    

let g topfun =
  List.map compile_fun topfun

let var_to_fun vars =
  (MyUtil.undefined ())

(*
  f : Closure.fundef list -> Closure.topvar list
        -> fundef list * Id.l list * (float * Id.l)
  戻り値は後ろから順に処理されていって欲しい. 具体的に言うとList.rev_iterとかで
*)
let f topfuns top_vars =
  let funs = g topfuns in
  let main, lst = var_to_fun top_vars in
    main :: funs, lst

