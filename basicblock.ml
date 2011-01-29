module VA = VirtualAsm
module EA = EscapeAnalysis

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

  | TupleAlloc of bool * (Id.t * VA.ty) list
  | ArrayAlloc of bool * VA.ty * Id.t


type fundef = { name : Id.l; args : (Id.t * VA.ty) list; body : ins list; ret : VA.ty; block_labels : Id.l list }

let counter : int ref = ref 0
let mkblockname () =
  let i = !counter in
    counter := !counter + 1;
    Id.L (Format.sprintf "block%d" i)

let to_ins = function
  | EA.Nop -> Nop
  | EA.Set (lit) -> Set lit
  | EA.Mov (v) -> Mov v
  | EA.Neg (id) -> Neg id
  | EA.Add (s1, s2) -> Add (s1, s2)
  | EA.Sub (s1, s2) -> Sub (s1, s2)
  | EA.Mul (s1, s2) -> Mul (s1, s2)
  | EA.Div (s1, s2) -> Div (s1, s2)
  | EA.SLL (s1, s2) -> SLL (s1, s2)
  | EA.SLR (s1, s2) -> SLR (s1, s2)
  | EA.Ld (m) -> Ld m
  | EA.St (data, mem) -> St (data, mem)
  | EA.FMov (id) -> FMov id
  | EA.FNeg (id) -> FNeg id
  | EA.FAdd (s1, s2) -> FAdd (s1, s2)
  | EA.FSub (s1, s2) -> FSub (s1, s2)
  | EA.FMul (s1, s2) -> FMul (s1, s2)
  | EA.FDiv (s1, s2) -> FDiv (s1, s2)
  | EA.FLd (m) -> FLd m
  | EA.FSt (data, mem) -> FSt (data, mem)

  | EA.BLd (m) -> BLd m
  | EA.BSt (data, mem) -> BSt (data, mem)

  | EA.Comp (op, ty, s1, s2) -> Comp (op, ty, s1, s2)

  | EA.ApplyCls (cls, args) -> ApplyCls (cls, args)
  | EA.ApplyDir (func, args) -> ApplyDir (func, args)

  | EA.Cons (h, t) -> Cons (h, t)
  | EA.Car (l) -> Car l
  | EA.Cdr (l) -> Cdr l
  | EA.FCons (h, t) -> FCons (h, t)
  | EA.FCar (l) -> FCar l
  | EA.FCdr (l) -> FCdr l

  | EA.TupleAlloc (escaped, data) -> TupleAlloc (escaped, data)
  | EA.ArrayAlloc (escaped, ty, num) -> ArrayAlloc (escaped, ty, num)


let rec linerize blocks stack = function
  | EA.Ans (EA.If (cond, tr, fal)) ->
      let block_tr = mkblockname () in
	begin match stack with
	  | [] ->
	      blocks := M.add block_tr (Label block_tr :: linerize blocks [] tr) !blocks;
	      If (to_ins cond,  block_tr) :: linerize blocks [] fal
	  | continue :: next ->
	      blocks := M.add block_tr (Label block_tr :: (List.rev (Jump continue  :: (List.rev (linerize blocks next tr))))) !blocks;
	      If (to_ins cond, block_tr) :: linerize blocks next fal
	end
  | EA.Ans (t) -> [to_ins t]
  | EA.Seq (t1, t2) -> 
      let continue = mkblockname () in
	linerize blocks (continue :: stack) t1 @ (Label (continue) :: linerize blocks stack t2)
  | EA.Let (id, exp, next) ->
      Let (id, to_ins exp) :: linerize blocks stack next

let rec add_ret = function
  | [Jump label] as i -> i
  | [] -> [Ret]
  | ins :: tail -> ins :: add_ret tail


let linerize_func {EA.name = func_label; EA.args = args; EA.body = body; EA.ret = ret } =
  let blocks = ref M.empty in
  let insts = linerize blocks [] body in
  let insts' = add_ret insts in
  let blocknames, inst = M.fold (fun key ins ret ->
				   let block, inst = ret in
				     key :: block, add_ret ins @ inst)
                                !blocks ([], []) in
    { name = func_label; args = args; body = Label func_label :: Entry :: insts' @ inst; ret = ret; block_labels = blocknames }

let f funs =
  List.map linerize_func funs
