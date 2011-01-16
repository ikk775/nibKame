type ins =
  | Let of (Id.t * VirtualAsm.ty) * ins
  | Entry
  | Ret
  | Jump of Id.l
  | Label of Id.l
  | Nop
  | Set of VirtualAsm.literal
  | Mov of VirtualAsm.id_or_imm
  | Neg of Id.t
  | Add of Id.t * VirtualAsm.id_or_imm
  | Sub of Id.t * VirtualAsm.id_or_imm
  | Mul of Id.t * VirtualAsm.id_or_imm
  | Div of Id.t * VirtualAsm.id_or_imm
  | SLL of Id.t * VirtualAsm.id_or_imm
  | SLR of Id.t * VirtualAsm.id_or_imm
  | Ld of VirtualAsm.mem_op
  | St of Id.t * VirtualAsm.mem_op
  | FMov of Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | FLd of VirtualAsm.mem_op
  | FSt of Id.t * VirtualAsm.mem_op
  | BLd of VirtualAsm.mem_op
  | BSt of Id.t * VirtualAsm.mem_op
  | Comp of VirtualAsm.cmp_op * VirtualAsm.ty * Id.t * VirtualAsm.id_or_imm
  | If of ins * Id.l
  | ApplyCls of Id.t * Id.t list
  | ApplyDir of Id.l * Id.t list
  | Cons of Id.t * Id.t
  | Car of Id.t
  | Cdr of Id.t 
  | FCons of Id.t * Id.t
  | FCar of Id.t
  | FCdr of Id.t
  | TupleAlloc of (Id.t * VirtualAsm.ty) list
  | ArrayAlloc of VirtualAsm.ty * Id.t

type fundef = { name : Id.l; args : (Id.t * VirtualAsm.ty) list; body : ins list; ret : VirtualAsm.ty; block_labels : Id.l list }

val f: VirtualAsm.fundef list -> fundef list
