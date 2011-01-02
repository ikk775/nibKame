module VA = VirtualAsm

type block =
  { blockname : Id.l; body : ins list }

type ins =
  | Let of (Id.t * VA.ty) * ins
  | Ret of Id.t
  | Entry
  | Jump of Id.l
  | Label of Id.l

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

  | ApplyCls of Id.t * Id.t list
  | ApplyDir of Id.l * Id.t list

  | ArrayRef of Id.t * Id.t
  | ArraySet of Id.t * Id.t * Id.t

  | Cons of Id.t * Id.t
  | Car of Id.t
  | Cdr of Id.t 
  | FCons of Id.t * Id.t
  | FCar of Id.t
  | FCdr of Id.t

  | TupleAlloc of (Id.t * ty) list
  | ArrayAlloc of ty * Id.t

  | Save of Id.t * Id.t
  | Restore of Id.t * Id.t

type fundef { name : Id.l; args ; (Id.t * VA.ty) list; body : ins list; ret : VA.ty }

let counter : int ref = ref 0
let mkblockname () =
  let i = counter in
    incf counter;
    Format.sprintf "block:%d" i

let rec to_ins stack = function
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
  | VA.If (cond, tr, fal) ->
      let newblock = mkblockname () in
	begin match stack with
	  | [] -> If (to_ins cond, Label newblock :: linerize [] tr) :: linerize [] fal
	  | continue :: next ->
	      If (to_ins cond, Label newblock :: (List.rev (Jump continue  :: (List.rev (linerize next tr))))) :: linerize next fal
	end
  | VA.ApplyCls (cls, args) -> ApplyCls (cls, args)
  | VA.ApplyDir (func, args) -> ApplyDir (func, args)

  | VA.ArrayRef (ary, idx) -> ArrayRef (ary, idx)
  | VA.ArraySet (ary, idx, data) -> ArraySet (ary, idx, data)

  | VA.Cons (h, t) -> Cons (h, t)
  | VA.Car (l) -> Car l
  | VA.Cdr (l) -> Cdr l
  | VA.FCar (l) -> FCar l
  | VA.FCdr (l) -> FCdr l

  | VA.TupleAlloc (data) -> TupleAlloc data
  | VA.ArrayAlloc (ty, num) -> ArrayAlloc (ty, num)

  | VA.Save (dst, data) -> Save (dst, data)
  | VA.Restore (src, dst) -> Restore (src, dst)

and linerize stack = function
  | VA.Ans (t) -> [to_ins stack t]
  | VA.Seq (t1, t2) -> 
      let continue = mkblockname () in
	linerize (continue :: stack) t1 @ (Label (continue) :: linerize stack t2)
  | VA.Let (id, exp, next) ->
      Let (id, to_ins exp) :: linerize stack next

let linerizr_func {VA.name = Id.L func_label; VA.args = args; VA.body = body } =
  MyUtil.undefined ()
