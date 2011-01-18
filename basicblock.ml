module VA = VirtualAsm

module M = 
  Map.Make
    (struct
       type t = Id.l
       let compare = compare
     end)

type ins =
  | Let of (Id.t * VA.ty) * ins
  | Entry
  | Ret
  | Jump of Id.l
  | Label of Id.l
  | Nop

  | Set of VA.literal
  | Mov of VA.id_or_imm
  | Neg of Id.t
  | Add of Id.t * VA.id_or_imm
  | Sub of Id.t * VA.id_or_imm
  | Mul of Id.t * VA.id_or_imm (* imul *)
  | Div of Id.t * VA.id_or_imm (* cdq and idiv *)
  | SLL of Id.t * VA.id_or_imm
  | SLR of Id.t * VA.id_or_imm
  | Ld of VA.mem_op (* load to *)
  | St of Id.t * VA.mem_op (* store *)
  | FMov of Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | FLd of VA.mem_op
  | FSt of Id.t * VA.mem_op

  | BLd of VA.mem_op
  | BSt of Id.t * VA.mem_op

  | Comp of VA.cmp_op * VA.ty * Id.t * VA.id_or_imm
  | If of ins * Id.l (* 真のときのジャンプ先 *)

  | ApplyCls of (Id.t * VA.ty) * Id.t list
  | ApplyDir of (Id.l * VA.ty)* Id.t list

  | Cons of Id.t * Id.t
  | Car of Id.t
  | Cdr of Id.t 
  | FCons of Id.t * Id.t
  | FCar of Id.t
  | FCdr of Id.t

  | TupleAlloc of (Id.t * VA.ty) list
  | ArrayAlloc of VA.ty * Id.t


type fundef = { name : Id.l; args : (Id.t * VA.ty) list; body : ins list; ret : VA.ty; block_labels : Id.l list }

let counter : int ref = ref 0
let mkblockname () =
  let i = !counter in
    counter := !counter + 1;
    Id.L (Format.sprintf "block:%d" i)

let to_ins = function
  | VA.Nop -> Nop
  | VA.Set (lit) -> Set lit
  | VA.Mov (v) -> Mov v
  | VA.Neg (id) -> Neg id
  | VA.Add (s1, s2) -> Add (s1, s2)
  | VA.Sub (s1, s2) -> Sub (s1, s2)
  | VA.Mul (s1, s2) -> Mul (s1, s2)
  | VA.Div (s1, s2) -> Div (s1, s2)
  | VA.SLL (s1, s2) -> SLL (s1, s2)
  | VA.SLR (s1, s2) -> SLR (s1, s2)
  | VA.Ld (m) -> Ld m
  | VA.St (data, mem) -> St (data, mem)
  | VA.FMov (id) -> FMov id
  | VA.FNeg (id) -> FNeg id
  | VA.FAdd (s1, s2) -> FAdd (s1, s2)
  | VA.FSub (s1, s2) -> FSub (s1, s2)
  | VA.FMul (s1, s2) -> FMul (s1, s2)
  | VA.FDiv (s1, s2) -> FDiv (s1, s2)
  | VA.FLd (m) -> FLd m
  | VA.FSt (data, mem) -> FSt (data, mem)

  | VA.BLd (m) -> BLd m
  | VA.BSt (data, mem) -> BSt (data, mem)

  | VA.Comp (op, ty, s1, s2) -> Comp (op, ty, s1, s2)

  | VA.ApplyCls (cls, args) -> ApplyCls (cls, args)
  | VA.ApplyDir (func, args) -> ApplyDir (func, args)

  | VA.Cons (h, t) -> Cons (h, t)
  | VA.Car (l) -> Car l
  | VA.Cdr (l) -> Cdr l
  | VA.FCons (h, t) -> FCons (h, t)
  | VA.FCar (l) -> FCar l
  | VA.FCdr (l) -> FCdr l

  | VA.TupleAlloc (data) -> TupleAlloc data
  | VA.ArrayAlloc (ty, num) -> ArrayAlloc (ty, num)


let rec linerize blocks stack = function
  | VA.Ans (VA.If (cond, tr, fal)) ->
      let block_tr = mkblockname () in
	begin match stack with
	  | [] ->
	      blocks := M.add block_tr (Label block_tr :: linerize blocks [] tr) !blocks;
	      If (to_ins cond,  block_tr) :: linerize blocks [] fal
	  | continue :: next ->
	      blocks := M.add block_tr (Label block_tr :: (List.rev (Jump continue  :: (List.rev (linerize blocks next tr))))) !blocks;
	      If (to_ins cond, block_tr) :: linerize blocks next fal
	end
  | VA.Ans (t) -> [to_ins t]
  | VA.Seq (t1, t2) -> 
      let continue = mkblockname () in
	linerize blocks (continue :: stack) t1 @ (Label (continue) :: linerize blocks stack t2)
  | VA.Let (id, exp, next) ->
      Let (id, to_ins exp) :: linerize blocks stack next

let rec add_ret = function
  | [Jump label] as i -> i
  | [] -> [Ret]
  | ins :: tail -> ins :: add_ret tail


let linerize_func {VA.name = func_label; VA.args = args; VA.body = body; VA.ret = ret } =
  let blocks = ref M.empty in
  let insts = linerize blocks [] body in
  let blocknames, inst = M.fold (fun key ins ret ->
				   let block, inst = ret in
				     key :: block, ins @ inst)
                                !blocks ([], []) in
    { name = func_label; args = args; body = Label func_label :: insts @ inst; ret = ret; block_labels = blocknames }

let f funs =
  List.map linerize_func funs
