
type id_or_imm = V of Id.t | C of int
type cmp_op = Eq | NotEq | LsEq | Ls | Gt | GtEq

type literal = Int_l of int | Char_l of char | Pointer_l of Id.l | Nil of Type.listCategory

type p_type = Tuple of ty list | Array of ty | List of ty | Undefined
and ty = Char | Int | Float | Fun of ty list * ty | Pointer of p_type

type mem_op =
  | Direct of Id.t
  | Label of Id.l
  | Plus_offset of Id.t * id_or_imm
  | Scaled_offset of Id.t * Id.t * int

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
  | Mul of Id.t * id_or_imm
  | Div of Id.t * id_or_imm
  | SLL of Id.t * id_or_imm
  | SLR of Id.t * id_or_imm
  | Ld of mem_op 
  | St of Id.t * mem_op 
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
  | If of exp * t * t
  | ApplyCls of (Id.t * ty) * Id.t list
  | ApplyDir of (Id.l * ty) * Id.t list
  | Cons of Id.t * Id.t
  | Car of Id.t
  | Cdr of Id.t  | FCons of Id.t * Id.t
  | FCar of Id.t
  | FCdr of Id.t
  | TupleAlloc of (Id.t * ty) list
  | ArrayAlloc of ty * Id.t


type fundef = { name: Id.l; args: (Id.t * ty) list; body: t; ret: ty }
val genid : unit -> Id.t
val temp : unit -> Id.t
val tuple_size : ty list -> int
val array_size : int -> ty -> int
val sizeof : ty -> int
val f: Closure.topDecl list -> fundef list * (float * Id.l) list
val var_labels: Id.Set.t ref
