
type id_or_imm = V of Id.t | C of int
type cmo_op =
  | Eq | NotEq | LsEq | Ls | Gt | GtEq

type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp =
  | Nop
  | SetL of Id.l
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

type fundef = { name: Id.l; args: Id.t list; fargs: Id.t list; body: t; ret: Type.t }
