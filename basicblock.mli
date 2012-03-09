
module M :
  sig
    type key = Id.l
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end

type ins =
    Let of (Id.t * VirtualAsm.ty) * ins
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
  | If of ins * Id.l * Id.l
  | ApplyCls of (Id.t * VirtualAsm.ty) * Id.t list
  | ApplyDir of (Id.l * VirtualAsm.ty) * Id.t list
  | Cons of Id.t * Id.t
  | Car of Id.t
  | Cdr of Id.t
  | FCons of Id.t * Id.t
  | FCar of Id.t
  | FCdr of Id.t
  | TupleAlloc of (Id.t * VirtualAsm.ty) list
  | ArrayAlloc of VirtualAsm.ty * Id.t

type fundef = {
  name : Id.l;
  args : (Id.t * VirtualAsm.ty) list;
  body : ins list M.t;
  ret : VirtualAsm.ty;
}

val f : VirtualAsm.fundef list -> fundef list
