module VA = VirtualAsm

type reg =
  | EAX | EDX | ECX | EBX | ESI | EDI | EBP | ESP | TempR of Id.t

type freg =
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | TempF of Id.t

type mem =
  | Direct of Id.l
  | Base of reg
  | Offset of reg * int
  | OffsetL of reg * Id.l
  | Index of int * reg * int 
  | IndexL of Id.l * reg * int
  | RcdAry of reg * reg * int

type imm = VA.literal

type rmi =
  | R of reg
  | M of mem
  | I of imm

type twoOp =
  | RR of reg * reg
  | RM of reg * mem
  | MR of mem * reg
  | RI of reg * imm
  | MI of mem * imm

let cnt0 = ref 0
let tempR () =
  let i = !cnt0 in
    incr cnt0;
    TempR (Format.sprintf "tmpr%d" i)
let cnt1 = ref 0
let tempF () =
  let i = !cnt0 in
    incr cnt1;
    TempF (Format.sprintf "tmpf%d" i)


type inst =
  | Mov of reg * reg
  | Xchg of reg * reg
  | FMov of freg * freg
  | St of mem * reg
  | FSt of mem *freg
  | Ld of reg * mem
  | FLd of freg * mem
  | Set of reg * VA.literal
  | SetM of mem * VA.literal
  | Lea of reg * mem (* Load Effective Address *)

  | Push of rmi
  | Pop of reg

  | Add of twoOp
  | Sub of twoOp
  | Mul of reg * rmi
  | CDQ (* Extend EAX to EDX *)
  | Div of reg (* divided edx:eax by reg *)

  | FAdd of freg * freg
  | FSub of freg * freg
  | FMul of freg * freg
  | FDiv of freg * freg
  | Fsqrd of freg * freg

  | F2I of freg * reg
  | I2F of reg * freg

  | SAL of twoOp (* Shift Arithmetic Left *)
  | SHL of twoOp
  | SAR of twoOp (* Shift Arithmetic Right *)
  | SHR of twoOp

  | Cmp of twoOp
  | Branch of VA.cmp_op * Id.l
  | Jmp of Id.l
  | Call of Id.l
  | Leave
  | Ret

let str_of_reg = function
  | EAX -> "%eax"
  | EDX -> "%edx"
  | ECX -> "%ecx"
  | EBX -> "%ebx"
  | ESI -> "%esi"
  | EDI -> "%edi"
  | EBP -> "%ebp"
  | ESP -> "%esp"
  | TempR s -> raise Failure (Format.sprintf "Not assigned register, %s" s)

let str_of_freg = function
  | XMM0 -> "%xmm0"
  | XMM1 -> "%xmm1"
  | XMM2 -> "%xmm2"
  | XMM3 -> "%xmm3"
  | XMM4 -> "%xmm4"
  | XMM5 -> "%xmm5"
  | XMM6 -> "%xmm6"
  | XMM7 -> "%xmm7"
  | TempF s -> raise Failure (Format.sprintf "Not assigned register, %s" s)

let isscale = function
  | 1 | 2 | 4 | 8 -> true
  | _ -> false

let str_of_mem = function
  | Direct (Id.L label) -> label
  | Base reg -> Format.sprintf "(%s)" (str_of_reg reg)
  | Offset (base, offset) -> Format.sprintf "%d(%s)" offset (str_of_reg base)
  | OffsetL (base, Id.L disp) -> Format.sprintf "%s(%s)" disp (str_of_reg base)
  | Index (disp, index, scale) when isscale scale -> Format.sprintf "%d(%s,%d)" disp (str_of_reg index) scale
  | Index _ -> raise Failure "Index scale is used with 1, 2, 4, 8 only."
  | IndexL (Id.L disp, index, scale) when isscale scale -> Format.sprintf "%s(%s,%d)" d disp (str_of_reg index) scale
  | IndexL _ -> raise Failure "Index scale is used with 1, 2, 4, 8 only." 
  | RcdAry (base, index, disp) -> Format.sprintf "%d(%s,%s)" disp (str_of_reg base) (str_of_reg index)

let str_of_imm : imm -> string = function
    | VA.Int_l i -> Format.sprintf "$%d" i
    | VA.Char_l c -> Format.sprintf "\'%c\'" c
    | VA.Pointer_l (Id.L label) -> label

let str_of_rmi = function
  | R reg -> str_of_reg reg
  | M mem -> str_of_mem mem
  | I imm -> str_of_imm imm

let str_of_twoOp = function
  | RR (dst, src) -> Format.sprintf "%s, %s" (str_of_reg src) (str_of_reg dst)
  | RM (dst, src) -> Format.sprintf "%s, %s" (str_of_mem src) (str_of_reg dst)
  | MR (dst, src) -> Format.sprintf "%s, %s" (str_of_reg src) (str_of_mem dst)
  | RI (dst, src) -> Format.sprintf "%s, %s" (str_of_imm src) (str_of_reg dst)
  | MI (dst, src) -> Format.sprintf "%s, %s" (str_of_imm src) (str_of_mem dst)

