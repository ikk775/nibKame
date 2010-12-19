
type id_or_imm = V of Id.t | C of int
type cmp_op = Eq | NotEq | LsEq | Ls | Gt | GtEq

type literal = Int_l of int | Char_l of char | Pointer_l of Id.l

type ty = Int | Float | Char | Pointer of Type.t | Fun

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
val genid : unit -> Id.t
val temp : unit - Id.t
val f: Closure.topDecl -> fundef list * (float * Id.l) list
