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
  | VA.Eq -> Eq | VA.NotEq -> NotEq | VA.LsEq -> LsEq | VA.Ls -> Ls | VA.Gt -> Gt | VA.GtEq -> GtEq

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
  | BSt of mem * reg
  | FSt of mem *freg
  | Ld of reg * mem
  | FLd of freg * mem
  | Set of reg * imm
  | SetM of mem * imm
  | Lea of reg * mem (* Load Effective Address *)

  | Push of rmi
  | Pop of reg
  | FPush of freg
  | FPop of freg

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

  | F2I of reg * freg
  | I2F of freg * reg

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
  | RcdAry (base, index, scale) when isscale scale -> Format.sprintf "(%s,%s,%d)" (str_of_reg base) (str_of_reg index) scale
  | RcdAry _ -> failwith "Index scale is used with 1, 2, 4, 8 only."
  | TempM s -> failwith (Format.sprintf "Not assigned address, %s" s)

let to_mem = function
  | VA.Direct r -> Base (TempR r)
  | VA.Label l -> Direct l
  | VA.Plus_offset (t1, VA.V t2) -> RcdAry (TempR t1, TempR t2, 1)
  | VA.Plus_offset (t1, VA.C t2) -> Offset (TempR t1, t2)
  | VA.Scaled_offset (base, index, scale) -> RcdAry (TempR base, TempR index, scale)

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

let char_store mem src =
  Format.sprintf "movb %s, %s"
    (match src with
       | EAX -> "%al"
       | EBX -> "%bl"
       | ECX -> "%cl"
       | EDX -> "%dl"
       | _ -> failwith "unsupported register to charactor operation")
    (str_of_mem mem)


let str_of_inst = function
  | Mov (dst, src) -> Format.sprintf "movl %s, %s" (str_of_reg src) (str_of_reg dst)
  | Xchg (dst, src) -> Format.sprintf "xchgl %s, %s" (str_of_reg src) (str_of_reg dst)
  | FMov (dst, src) -> Format.sprintf "movsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | St (mem, src) -> Format.sprintf "movl %s, %s" (str_of_reg src) (str_of_mem mem)
  | BSt (mem, src) -> char_store mem src
  | FSt (mem, src) -> Format.sprintf "movsd %s, %s" (str_of_freg src) (str_of_mem mem)
  | Ld (dst, mem) -> Format.sprintf "movl %s, %s" (str_of_mem mem) (str_of_reg dst)
  | FLd (dst, mem) -> Format.sprintf "movsd %s, %s" (str_of_mem mem) (str_of_freg dst)
  | Set (dst, imm) -> Format.sprintf "movl %s, %s" (str_of_imm imm) (str_of_reg dst)
  | SetM (mem, imm) -> Format.sprintf "movl %s, %s" (str_of_imm imm) (str_of_mem mem)
  | Lea (dst, mem) -> Format.sprintf "leal %s, %s" (str_of_mem mem) (str_of_reg dst)
  | Push (src) -> Format.sprintf "pushl %s" (str_of_rmi src)
  | Pop (dst) -> Format.sprintf "popl %s" (str_of_reg dst)
  | FPush (src) -> Format.sprintf "pushl %s" (str_of_freg src)
  | FPop (dst) -> Format.sprintf "popl %s" (str_of_freg dst)

  | Add op -> Format.sprintf "addl %s" (str_of_twoOp op)
  | Sub op -> Format.sprintf "subl %s" (str_of_twoOp op)
  | Mul (dst, src) -> Format.sprintf "imul %s, %s" (str_of_rmi src) (str_of_reg dst)
  | CDQ -> "cdq"
  | Div reg -> Format.sprintf "idiv %s" (str_of_reg reg)

  | INC op -> Format.sprintf "incl %s" (str_of_rmi op)
  | DEC op -> Format.sprintf "decl %s" (str_of_rmi op)

  | AND op -> Format.sprintf "andl %s" (str_of_twoOp op)
  | OR op -> Format.sprintf "orl %s" (str_of_twoOp op)
  | XOR op -> Format.sprintf "xorl %s" (str_of_twoOp op)
  | NEG reg -> Format.sprintf "negl %s" (str_of_reg reg)
  | NOT reg -> Format.sprintf "notl %s" (str_of_reg reg)

  | FAdd (dst, src) -> Format.sprintf "addsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | FSub (dst, src) -> Format.sprintf "subsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | FMul (dst, src) -> Format.sprintf "mulsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | FDiv (dst, src) -> Format.sprintf "divsd %s, %s" (str_of_freg src) (str_of_freg dst)
  | Fsqrd (dst, src) -> Format.sprintf "sqrtsd %s, %s" (str_of_freg src) (str_of_freg dst)
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
			| Eq -> "je %s" label
			| NotEq -> "jne %s" label
			| LsEq -> "jle %s" label
			| Ls -> "jl %s" label
			| Gt -> "jg %s" label
			| GtEq -> "jge %s" label
			| Zero -> "jz %s" label
			| NotZero -> "jnz %s" label)
  | Jmp (Id.L label) -> Format.sprintf "jmp %s" label
  | Call (Id.L label) -> Format.sprintf "call %s" label 
  | Label (Id.L label) -> Format.spintf "%s:" label
  | Leave -> "leave"
  | Ret -> "ret"


let twoOp_to_twoOp dst src =
  match src with
    | VA.V s -> RR (TempR dst, TempR s)
    | VA.C s -> RI (TempR dst, VA.Int_l s)

let id_or_imm_to_rmi = function
  | VA.V s -> R (TempR s)
  | VA.C s -> I (VA.Int_l s)

let get_dst = function
  | BB.Let _ -> MyUtil.undefined ()
  | BB.Entry | BB.Ret | BB.Jump _ | BB.Label _ | BB.Nop -> None
  | BB.Set _ | BB.Mov _ -> None
  | BB.Neg dst | BB.Add (dst, _) | BB.Sub (dst, _) | BB.Mul (dst, _) | BB.Div (dst, _) | BB.SLL (dst, _) | BB.SLR (dst, _) -> Some dst
  | BB.Ld _ | BB.St _ -> None
  | BB.FMov _ -> None
  | BB.FNeg dst | BB.FAdd (dst, _) | BB.FSub (dst, _)  | BB.FMul (dst, _) | BB.FDiv (dst, _) -> Some dst
  | BB.FLd _ | BB.FSt _ -> None
  | BB.BLd _ | BB.BSt _ -> None
  | BB.Comp _ -> None
  | BB.If _ -> MyUtil.undefined ()
  | BB.ApplyCls _ | BB.ApplyDir _ -> None
  | BB.Cons _ | BB.Car _ | BB.Cdr _ | BB.FCons _ | BB.FCar _ | BB.FCdr _ -> None
  | BB.TupleAlloc _ | BB.ArrayAlloc _ -> None

(* 関数の戻り値の型が必要 *)
let get_type = function
  | BB.Let _ | BB.Entry | BB.Ret | BB.Jump _ | BB.Label _ | BB.Nop -> None
  | BB.Set _ | BB.Mov _ | BB.Neg _ | BB.Add _ | BB.Sub _ | BB.Mul _ | BB.Div _ | BB.SLL _ | BB.SLR _ | BB.Ld _ -> Some VA.Int
  | BB.St _ -> None
  | BB.FMov _ | BB.FNeg _ | BB.FAdd _ | BB.FSub _ | BB.FMul _ | BB.FDiv _ | BB.FLd _ -> Some VA.Float
  | BB.FSt _ -> None
  | BB.BLd _ -> Some VA.Char
  | BB.BSt _ -> None
  | BB.Comp _ -> Some VA.Int
  | BB.If _ -> None
  | BB.ApplyCls _ | BB.ApplyDir _ -> MyUtil.undefined ()
  | BB.Cons _ | BB.Car _ | BB.Cdr _ -> Some (VA.Pointer (VA.List VA.Int))
  | BB.FCons _ | BB.FCar _ | BB.FCdr _ -> Some (VA.Pointer (VA.List VA.Float))
  | BB.TupleAlloc l -> Some (VA.Pointer (VA.Tuple (List.map snd l)))
  | BB.ArrayAlloc (t, _) -> Some (VA.Pointer (VA.Array t))

(* 演算の値が返る部分は Let の子になるよう調節する．*)
let rec asmgen = function
  | [] -> []
  | BB.Ret :: tail -> Leave :: Ret :: asmgen tail
  | BB.Entry :: tail -> Entry :: asmgen tail
  | BB.Jump l :: tail -> Jump l :: asmgen tail
  | BB.Label l :: tail -> Label l :: asmgen tail
  | BB.Nop :: tail -> Nop :: asmgen tail

  (* 複数の命令に対して最適化を行う部分 *)
  (*| BB.Let ((dst, VA.Float), ins) :: tail -> *)

  | BB.Let ((dst, t), ins) :: tail ->
      begin match ins with (* 命令選択の本体にあたる *)

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
	      | VA.Pointer t when op = VA.Eq ->
		  let func = match t with
		    | VA.Array _ -> Id.L "_nibkame_array_compare_"
		    | VA.List _ -> Id.L "_nibkame_list_compare_"
		    | VA.Tuple _ -> Id.L "_nibkame_tuple_comare_"
		    | VA.undefined -> Id.L "_nibkame_generic_comare_"
		  in
		    Push (id_or_imm_to_rmi src2) :: Push (TempR src1) :: Call (func)
		    :: Test (RR (EAX, EAX)) :: Branch (Zero, b_label) :: asmgen tail
	      | VA.Pointer t when op = VA.NotEq ->
		  let func = match t with
		    | VA.Array _ -> Id.L "_nibkame_array_compare_"
		    | VA.List _ -> Id.L "_nibkame_list_compare_"
		    | VA.Tuple _ -> Id.L "_nibkame_tuple_comare_"
		    | VA.undefined -> Id.L "_nibkame_generic_comare_"
		  in
		    Push (id_or_imm_to_rmi src2) :: Push (TempR src1) :: Call (func)
		    :: Test (RR (EAX, EAX)) :: Branch (NotZero, b_label) :: asmgen tail
	      | VA.Pointer _ | VA.Fun -> failwith "Not Supported Compare type."
	    end
	| ins ->
	    let t = VA.temp () in
	      asmgen (BB.Let ((t, VA.Int), ins) :: BB.If ((BB.Comp (VA.NotEq, VA.Int, t, VA.C 0)) b_label) :: tail)
      end

  (* 一時変数への副作用を持たないもの *)
  | BB.St (src, mem) :: tail -> St (to_mem mem, TempR src) :: asmgen tail
  | BB.FSt (src, mem) :: tail -> FSt (to_mem mem, TempF src) :: asmgen tail
  | BB.BSt (src, mem) :: tail -> BSt (to_mem mem, TempR src) :: asmgen tail

  (* 全てに代入先を用意 *)
  | single :: tail ->
      begin match get_type single with
	| Some t -> asmgen (BB.Let ((VA.temp (), t), single) :: tail)
	| None -> MyUtil.undefined () (* 全て対応したので到達しない筈． *)
      end

