module VA = VirtualAsm
module BB = Basicblock

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
  | TempM of Id.t

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

type cmp_op = Eq | NotEq | LsEq | Ls | Gt | GtEq | Zero | NotZero

let to_cmp = function
  | VA.Eq -> Eq | VA.NotEq -> NotEq | VA.LsEq -> LsEQ | VA.Ls -> Ls | VA.Gt -> Gt | VA.GtEq -> GtEq

let cnt0 = ref 0
let tempR () =
  let i = !cnt0 in
    incr cnt0;
    TempR (Format.sprintf "tmpr%d" i)
let cnt1 = ref 0
let tempF () =
  let i = !cnt1 in
    incr cnt1;
    TempF (Format.sprintf "tmpf%d" i)
let cnt2 = ref 0
let tempM () =
  let i = !cnt2 in
    incr cnt2;
    TempM (Format.sprintf "tmpm%d" i)

type inst =
  | Mov of reg * reg
  | Xchg of reg * reg
  | FMov of freg * freg
  | St of mem * reg
  | FSt of mem *freg
  | Ld of reg * mem
  | FLd of freg * mem
  | Set of reg * imm
  | SetM of mem * imm
  | Lea of reg * mem (* Load Effective Address *)

  | Push of rmi
  | Pop of reg

  | Add of twoOp
  | Sub of twoOp
  | Mul of reg * rmi
  | CDQ (* Extend EAX to EDX *)
  | Div of reg (* divided edx:eax by reg *)

  | INC of rmi
  | DEC of rmi

  | AND of twoOp
  | OR of twoOp
  | XOR of twoOp
  | NEG of reg
  | NOT of reg

  | FAdd of freg * freg
  | FSub of freg * freg
  | FMul of freg * freg
  | FDiv of freg * freg
  | Fsqrd of freg * freg
  | FComp of freg * freg

  | F2I of freg * reg
  | I2F of reg * freg

  | SAL of twoOp (* Shift Arithmetic Left *)
  | SHL of twoOp
  | SAR of twoOp (* Shift Arithmetic Right *)
  | SHR of twoOp

  | Cmp of twoOp
  | Test of twoOp
  | Branch of cmp_op * Id.l
  | Jmp of Id.l
  | Call of Id.l
  | Label of Id.l
  | Entry (* It is replaced to making stackframe in Register Allocation. *)
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
  | TempR s -> failwith (Format.sprintf "Not assigned register, %s" s)

let str_of_freg = function
  | XMM0 -> "%xmm0"
  | XMM1 -> "%xmm1"
  | XMM2 -> "%xmm2"
  | XMM3 -> "%xmm3"
  | XMM4 -> "%xmm4"
  | XMM5 -> "%xmm5"
  | XMM6 -> "%xmm6"
  | XMM7 -> "%xmm7"
  | TempF s -> failwith (Format.sprintf "Not assigned register, %s" s)

let isscale = function
  | 1 | 2 | 4 | 8 -> true
  | _ -> false

let str_of_mem = function
  | Direct (Id.L label) -> label
  | Base reg -> Format.sprintf "(%s)" (str_of_reg reg)
  | Offset (base, offset) -> Format.sprintf "%d(%s)" offset (str_of_reg base)
  | OffsetL (base, Id.L disp) -> Format.sprintf "%s(%s)" disp (str_of_reg base)
  | Index (disp, index, scale) when isscale scale -> Format.sprintf "%d(%s,%d)" disp (str_of_reg index) scale
  | Index _ -> failwith "Index scale is used with 1, 2, 4, 8 only."
  | IndexL (Id.L disp, index, scale) when isscale scale -> Format.sprintf "%s(%s,%d)" disp (str_of_reg index) scale
  | IndexL _ -> failwith "Index scale is used with 1, 2, 4, 8 only." 
  | RcdAry (base, index, disp) -> Format.sprintf "%d(%s,%s)" disp (str_of_reg base) (str_of_reg index)
  | TempM s -> failwith (Format.sprintf "Not assigned address, %s" s)

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

let str_of_inst = function
  | Mov (dst, src) -> Format.sprintf "movl %s, %s" (str_of_reg src) (str_of_reg dst)
  | Xchg (dst, src) -> Format.sprintf "xchgl %s, %s" (str_of_reg src) (str_of_reg dst)
  | FMov (dst, src) -> Format.sprintf "movsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | St (mem, src) -> Format.sprintf "movl %s, %s" (str_of_reg src) (str_of_mem mem)
  | FSt (mem, src) -> Format.sprintf "movsd %s, %s" (str_of_freg src) (str_of_mem mem)
  | Ld (dst, mem) -> Format.sprintf "movl %s, %s" (str_of_mem mem) (str_of_reg dst)
  | FLd (dst, mem) -> Format.sprintf "movsd %s, %s" (str_of_mem mem) (str_of_freg dst)
  | Set (dst, imm) -> Format.sprintf "movl %s, %s" (str_of_imm imm) (str_of_reg dst)
  | SetM (mem, imm) -> Format.sprintf "movl %s, %s" (str_of_imm imm) (str_of_mem mem)
  | Lea (dst, mem) -> Format.sprintf "leal %s, %s" (str_of_mem mem) (str_of_reg dst)
  | Push (src) -> Fotmat.sprintf "pushl %s" (str_of_rmi src)
  | Pop (dst) -> Format.sprintf "popl %s" (str_of_reg dst)

  | Add op -> Format.sprintf "addl %s" (str_of_twoOp op)
  | Sub op -> Format.sprintf "subl %s" (str_of_twoOp op)
  | Mul (dst, src) -> Format.sprintf "imul %s, %s" (str_of_reg src) (str_of_rmi dst)
  | CDQ -> "cdq"
  | Div reg -> Format.sprintf "idiv %s" (str_of_reg reg)

  | INC op -> Format.sprintf "incl %s" (str_of_rmi op)
  | DEC op -> Format.sprintf "decl %s" (str_of_rmi op)

  | AND op -> Format.sprintf "andl %s" (str_of_twoOp op)
  | OR op -> Format.sprintf "orl %s" (str_of_twoOp op)
  | XOR op -> Format.sprintf "xorl %s" (str_of_twoOp op)
  | NEG reg -> Format.sprintf "negl %s" (str_of_reg reg)
  | NOT reg -> Format.sprintf "notl %s" (str_of_reg reg)

  | Fadd (dst, src) -> Format.sprintf "addsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | Fsub (dst, src) -> Format.sprintf "subsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | Fmul (dst, src) -> Format.sprintf "mulsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | Fdiv (dst, src) -> Format.sprintf "divsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | Fsqrt (dst, src) -> Format.sprintf "sqrtsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | FComp (dst, src) -> Format.sprintf "comisd %s, %s" (str_of_freg src) (str_of_freg dst)

  | F2I (dst, src) -> Format.sprintf "cvtsi2sd %s, %s" (str_of_freg src) (str_of_reg dst)
  | I2F (dst, src) -> Format.sprintf "cvttsdi2si %s, %s" (str_of_reg src) (str_of_freg dst)

  | SAL op -> Format.sprintf "sall %s" (str_of_twoOp op)
  | SHL op -> Format.sprintf "shll %s" (str_of_twoOp op)
  | SAR op -> Format.sprintf "sarl %s" (str_of_twoOp op)
  | SHR op -> Format.sprintf "shrl %s" (str_of_twoOp op)

  | Cmp op -> Format.sprintf "cmp %s" (str_of_twoOp op)
  | Test op -> Format.sprintf "testl %s" (str_of_twoOp op)
  | Branch (cond, Id.L label) ->
      Format.sprintf (match cond with
			| Eq -> "je %s"
			| NotEq "jne %s"
			| LsEq -> "jle %s"
			| Ls -> "jl %s"
			| Gt -> "jg %s"
			| GtEq -> "jge %s"
			| Zero -> "jz %s"
			| NotZero -> "jnz %s") label
  | Jmp (Id.L label) -> Format.sprintf "jmp %s" label
  | Call (Id.L label) -> Format.sprintf "call %s" label 
  | Label (Id.L label) -> Format.spintf "%s:" label
  | Leave -> "leave"
  | Ret -> "ret"


let twoOp_to_twoOp dst src =
  match src with
    | V s -> RR (TempR dst, TempR s)
    | C s -> RI (TempR dst, VA.Int_l s)

let asmgen' = function
  | BB.Entry -> Entry, None
  | BB.Jump l -> Jump l, None
  | BB.Label l -> Label l, None
  | BB.Nop -> Nop, None

let rec asmgen = function
  | [] -> []
  | BB.Ret :: tail -> Leave :: Ret :: asmgen tail

  (* 複数の命令に対して最適化を行う部分 *)

  (* 複数の命令に変換される場合 *)
  | BB.Let ((dst, VA.Float), ins) :: tail ->

  | BB.Let ((dst, t), ins) :: tail ->
      begin match get_dist ins with
	|
      end

  | BB.If (ins, b_label) :: tail ->
      begin match ins with
	| BB.Comp (op, ty, src1, src2) ->
	    begin match ty with
	      | VA.Int | VA.Char ->
		  (match src2 with
		     | C 0 when op = VA.Eq -> Test (RI (TempR src1, Temp)) :: Branch (Zero, b_label)
		     | C 0 when op = VA.NotEq -> Test (RI (TempR src1, Temp)) :: Branch (NotZero, b_label)
		     | _ -> Cmp (twoOp_to_twoOp src1 src2) :: Branch (to_cmp_op op, b_label)) :: asmgen tail
	      | VA.Float -> let V s2 = src2 in FComp (TempF src1, TempF s2) :: Branch (to_cmp_op op, b_label) :: asmgen tail
	      | VA.Pointer t when op = VA.Eq -> MyUtil.undefined ()
	      | VA.Pointer
	      | VA.Fun -> MyUtil.undefind ()
	    end
	| ins ->
	    let t = BB.temp () in
	      asmgen (BB.Let ((t, VA.Int), ins) :: BB.If ((BB.Comp (VA.NotEq, VA.Int, t, VA.C 0)) b_label) :: tail)
      end

  | ApplyCls
  | ApplyDir
  | Cons
  | FCons
  | TupleAlloc
  | ArrayAlloc

  (* 一命令のみに対応する部分 *)
  | single :: tail -> 

