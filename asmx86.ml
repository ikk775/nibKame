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
  | Neg of reg

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
  | LoadFlagsToAH
  | Branch of cmp_op * Id.l
  | Jump of Id.l
  | Call of Id.l
  | Call_I of reg
  | Label of Id.l
  | Entry (* It is replaced to making stackframe in Register Allocation. *)
  | Leave
  | Ret
  (* 以下特殊な命令 *)
  | SETcc of VA.cmp_op * reg

type fundef = { name : Id.l; body : inst list }

let str_of_reg = function
  | EAX -> "%eax"
  | EDX -> "%edx"
  | ECX -> "%ecx"
  | EBX -> "%ebx"
  | ESI -> "%esi"
  | EDI -> "%edi"
  | EBP -> "%ebp"
  | ESP -> "%esp"
  | TempR s -> (* failwith (Format.sprintf "Not assigned register, %s" s) *)
      Format.sprintf "TempR.%s" s

let str_of_regb = function
  | EAX -> "%al"
  | EDX -> "%dl"
  | ECX -> "%cl"
  | EBX -> "%bl"
  | ESI | EDI | EBP | ESP as r -> failwith (Format.sprintf "Can't use with byte, %s" (str_of_reg r))
  | TempR s -> (* failwith (Format.sprintf "Not assigned register, %s" s) *)
      Format.sprintf "TempR.%s" s

let str_of_freg = function
  | XMM0 -> "%xmm0"
  | XMM1 -> "%xmm1"
  | XMM2 -> "%xmm2"
  | XMM3 -> "%xmm3"
  | XMM4 -> "%xmm4"
  | XMM5 -> "%xmm5"
  | XMM6 -> "%xmm6"
  | XMM7 -> "%xmm7"
  | TempF s -> (* failwith (Format.sprintf "Not assigned register, %s" s) *)
      Format.sprintf "TempF.%s" s

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
    | VA.Nil _ -> "0"

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
  | Neg op -> Format.sprintf "negl %s" (str_of_reg op)

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
  | I2F (dst, src) -> Format.sprintf "cvtsd2si %s, %s" (str_of_reg src) (str_of_freg dst)

  | SAL op -> Format.sprintf "sall %s" (str_of_twoOp op)
  | SHL op -> Format.sprintf "shll %s" (str_of_twoOp op)
  | SAR op -> Format.sprintf "sarl %s" (str_of_twoOp op)
  | SHR op -> Format.sprintf "shrl %s" (str_of_twoOp op)

  | Cmp op -> Format.sprintf "cmp %s" (str_of_twoOp op)
  | LoadFlagsToAH -> "lahf"
  | Test op -> Format.sprintf "testl %s" (str_of_twoOp op)
  | Branch (cond, Id.L label) ->
      begin match cond with
	| Eq -> Format.sprintf "je %s" label
	| NotEq -> Format.sprintf "jne %s" label
	| LsEq -> Format.sprintf "jle %s" label
	| Ls -> Format.sprintf "jl %s" label
	| Gt -> Format.sprintf "jg %s" label
	| GtEq -> Format.sprintf "jge %s" label
	| Zero -> Format.sprintf "jz %s" label
	| NotZero -> Format.sprintf "jnz %s" label
      end
  | Jump (Id.L label) -> Format.sprintf "jmp %s" label
  | Call (Id.L label) -> Format.sprintf "call %s" label 
  | Call_I adr -> Format.sprintf "call (%s)" (str_of_reg adr)
  | Label (Id.L label) -> Format.sprintf "%s:" label
  | Entry -> "# function entry."
  | Leave -> "leave"
  | Ret -> "ret"

  | SETcc (op, dst) ->
      begin match op with
	| VA.Eq -> Format.sprintf "sete %s" (str_of_regb dst)
	| VA.NotEq -> Format.sprintf "setne %s" (str_of_regb dst)
	| VA.LsEq -> Format.sprintf "setle %s" (str_of_regb dst)
	| VA.Ls -> Format.sprintf "setl %s" (str_of_regb dst)
	| VA.Gt -> Format.sprintf "setg %s" (str_of_regb dst)
	| VA.GtEq -> Format.sprintf "setge %s" (str_of_regb dst)
      end

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
  | BB.ApplyCls ((_, VA.Fun (arg, ret)), _) | BB.ApplyDir ((_, VA.Fun (arg, ret)), _) -> Some ret
  | BB.Cons _ | BB.Car _ | BB.Cdr _ -> Some (VA.Pointer (VA.List VA.Int))
  | BB.FCons _ | BB.FCar _ | BB.FCdr _ -> Some (VA.Pointer (VA.List VA.Float))
  | BB.TupleAlloc l -> Some (VA.Pointer (VA.Tuple (List.map snd l)))
  | BB.ArrayAlloc (t, _) -> Some (VA.Pointer (VA.Array t))

let is_power2 n =
  let n = abs n in
    n land (n - 1) = 0

let rec shift_width w n =
  if n land 1 = 1 
  then w
  else shift_width (w + 1) (n lsr 1)

(* 演算の値が返る部分は Let の子になるよう調節する．*)
let rec asmgen = function
  | [] -> []
  | BB.Ret :: tail -> Leave :: Ret :: asmgen tail
  | BB.Entry :: tail -> Entry :: asmgen tail
  | BB.Jump l :: tail -> Jump l :: asmgen tail
  | BB.Label l :: tail -> Label l :: asmgen tail
  | BB.Nop :: tail ->  asmgen tail

  | BB.Let ((dst, t), ins) :: tail ->
      begin match ins with
	| BB.Let _ | BB.Entry | BB.Ret | BB.Jump _ | BB.Label _ | BB.Nop -> failwith "unreconized instruction."
	| BB.Set imm -> Set (TempR dst, imm) :: asmgen tail
	| BB.Mov (VA.V src) -> Mov (TempR dst, TempR src) :: asmgen tail
	| BB.Mov (VA.C src) -> Set (TempR dst, VA.Int_l src) :: asmgen tail
	| BB.Neg src -> let t = TempR dst in Mov (t, TempR src) :: Neg t :: asmgen tail
	| BB.Add (src1, VA.C 1) -> let t = TempR dst in Mov (t, TempR src1) :: INC (R t) :: asmgen tail
	| BB.Add (src1, src2) -> let t = TempR dst in Mov (t, TempR src1) :: Add (twoOp_to_twoOp dst src2) :: asmgen tail
	| BB.Sub (src1, VA.C 1) -> let t = TempR dst in Mov (t, TempR src1) :: DEC (R t) :: asmgen tail
	| BB.Sub (src1, src2) -> let t = TempR dst in Mov (t, TempR src1) :: Sub (twoOp_to_twoOp dst src2) :: asmgen tail
	| BB.Mul (src1, VA.C src2) when is_power2 src2 -> let t = TempR dst in Mov (t, TempR src1) :: SAL (RI (t, VA.Int_l (shift_width 0 src2))) :: asmgen tail
	| BB.Mul (src1, VA.V src2) -> let t = TempR dst in Mov (t, TempR src1) :: Mul (t, (R (TempR src2))) :: asmgen tail
	| BB.Mul (src1, VA.C src2) -> let t = TempR dst in Mov (t, TempR src1) :: Mul (t, (I (VA.Int_l src2))) :: asmgen tail
	| BB.Div (src1, VA.C src2) when is_power2 src2 -> let t = TempR dst in Mov (t, TempR src1) :: SAR (RI (t, VA.Int_l (shift_width 0 src2))) :: asmgen tail
	| BB.Div (src1, VA.C src2) -> let t = tempR () in Mov (EAX, TempR src1) :: Set (t, VA.Int_l src2) :: CDQ :: Div t :: asmgen tail
	| BB.Div (src1, VA.V src2) -> Mov (EAX, TempR src1) :: CDQ :: Div (TempR src2) :: asmgen tail
	| BB.SLL (src1, src2) -> let t = TempR dst in Mov (t, TempR src1) :: SHL (twoOp_to_twoOp dst src2) :: asmgen tail
	| BB.SLR (src1, src2) -> let t = TempR dst in Mov (t, TempR src1) :: SHR (twoOp_to_twoOp dst src2) :: asmgen tail
	| BB.Ld (mem) -> Ld (TempR dst, to_mem mem) :: asmgen tail

	| BB.FMov (src) -> FMov (TempF dst, TempF src) :: asmgen tail
	| BB.FNeg (src) -> FSub (TempF dst, TempF dst) :: FSub (TempF dst, TempF src) :: asmgen tail
	| BB.FAdd (src1, src2) -> FMov (TempF dst, TempF src1) :: FAdd (TempF dst, TempF src2) :: asmgen tail
	| BB.FSub (src1, src2) -> FMov (TempF dst, TempF src1) :: FSub (TempF dst, TempF src2) :: asmgen tail
	| BB.FMul (src1, src2) -> FMov (TempF dst, TempF src1) :: FMul (TempF dst, TempF src2) :: asmgen tail
	| BB.FDiv (src1, src2) -> FMov (TempF dst, TempF src1) :: FDiv (TempF dst, TempF src2) :: asmgen tail
	| BB.FLd (mem) -> FLd (TempF dst, to_mem mem) :: asmgen tail
	| BB.BSt (src, mem) -> BSt (to_mem mem, TempR src) :: asmgen tail
	| BB.St (src, mem) -> St (to_mem mem, TempR src) :: asmgen tail
	| BB.FSt (src, mem) -> FSt (to_mem mem, TempF src) :: asmgen tail
	| BB.BLd (mem) -> Ld (TempR dst, to_mem mem) :: asmgen tail

	| BB.Comp (op, ty, src1, src2) ->
	    begin match ty with
	      | VA.Int | VA.Char ->
		  (match src2 with 
		     | VA.C 0 when op = VA.Eq -> Test (RR (TempR src1, TempR src1)) :: SETcc (op, TempR dst) :: asmgen tail
		     | VA.C 0 when op = VA.NotEq -> Test (RR (TempR src1, TempR src1)) :: SETcc (op, TempR dst) :: asmgen tail
		     | _ -> Cmp (twoOp_to_twoOp src1 src2) :: SETcc (op, TempR dst) :: asmgen tail)
	      | VA.Float -> let VA.V s2 = src2 in FComp (TempF src1, TempF s2) :: SETcc (op, TempR dst) :: asmgen tail
	      | VA.Pointer t when op = VA.Eq ->
		  let func = match t with
		    | VA.Array _ -> Id.L "_nibkame_array_compare_"
		    | VA.List _ -> Id.L "_nibkame_list_compare_"
		    | VA.Tuple _ -> Id.L "_nibkame_tuple_comare_"
		    | VA.Undefined -> Id.L "_nibkame_generic_comare_"
		  in
		    Push (id_or_imm_to_rmi src2) :: Push (R (TempR src1)) :: Call (func) :: Add (RI (ESP, VA.Int_l 8))
		    :: Test (RR (EAX, EAX)) :: SETcc (VA.Eq, TempR dst) :: asmgen tail
	      | VA.Pointer t when op = VA.NotEq ->
		  let func = match t with
		    | VA.Array _ -> Id.L "_nibkame_array_compare_"
		    | VA.List _ -> Id.L "_nibkame_list_compare_"
		    | VA.Tuple _ -> Id.L "_nibkame_tuple_comare_"
		    | VA.Undefined -> Id.L "_nibkame_generic_comare_"
		  in
		    Push (id_or_imm_to_rmi src2) :: Push (R (TempR src1)) :: Call (func) :: Add (RI (ESP, VA.Int_l 8))
		    :: Test (RR (EAX, EAX)) :: SETcc (VA.NotEq, TempR dst) :: asmgen tail
	      | VA.Pointer _ | VA.Fun _ -> failwith "Not Supported Compare type."
	    end

	| BB.If _ -> failwith "unreconized instruction."
	| BB.ApplyCls ((f, VA.Fun (t_args, ret)), args) -> 
	    begin match t with
	      | VA.Float -> List.fold_left (fun a b -> Push (R (TempR b)) :: a) (Push (R (TempR f)) ::  Call_I (TempR f) :: Add (RI (ESP, VA.Int_l ((VA.tuple_size t_args) + 4))):: FMov (TempF dst, XMM0) :: asmgen tail) args
	      | _ ->  List.fold_left (fun a b -> Push (R (TempR b)) :: a) (Push (R (TempR f)) ::  Call_I (TempR f) :: Add (RI (ESP, VA.Int_l ((VA.tuple_size t_args) + 4))) :: Mov (TempR dst, EAX) :: asmgen tail) args
	    end
	| BB.ApplyDir ((f, VA.Fun (t_args, ret)), args) -> 
	    begin match t with
	      | VA.Float -> List.fold_left (fun a b -> Push (R (TempR b)) :: a) (Call f :: Add (RI (ESP, VA.Int_l (VA.tuple_size t_args))) :: FMov (TempF dst, XMM0) :: asmgen tail) args
	      | _ -> List.fold_left (fun a b -> Push (R (TempR b)) :: a) (Call f :: Add (RI (ESP, VA.Int_l (VA.tuple_size t_args))) :: Mov (TempR dst, EAX) :: asmgen tail) args
	    end
	| BB.Cons (h, t) -> Push (R (TempR t)) :: Push (R (TempR h)) :: Call (Id.L "_nibkame_cons_") :: Add (RI (ESP, VA.Int_l 8)) :: Mov (TempR dst, EAX) :: asmgen tail
	| BB.Car l -> Ld (TempR dst, Base (TempR l)) :: asmgen tail
	| BB.Cdr l -> Ld (TempR dst, Offset (TempR l, 4)) :: asmgen tail
	| BB.FCons (h, t) -> Push (R (TempR t)) :: FPush (TempF h) :: Call (Id.L "_nibkame_fcons_") :: Add (RI (ESP, VA.Int_l 12)) :: FMov (TempF dst, XMM0) :: asmgen tail
	| BB.FCar l -> FLd (TempF dst, Base (TempR l)) :: asmgen tail
	| BB.FCdr l -> Ld (TempR dst, Offset (TempR l, 8)) :: asmgen tail
	| BB.TupleAlloc l ->
	    let stores, _ =
	      List.fold_left (fun a b -> 
				let src = fst b in
				  match snd b with
				    | VA.Float -> FSt (Offset (EAX, snd a), TempF src) :: fst a, 8 + (snd a)
				    | _ -> St (Offset (EAX, snd a), TempR src) :: fst a, 4 + (snd a))
		([], 0) l in
	      [Push (I (VA.Int_l (VA.tuple_size (List.map snd l)))); Call (Id.L "_nibkame_tuple_alloc_"); Add (RI (ESP, VA.Int_l 4))]
	      @ (List.rev stores) @ Mov (TempR dst, EAX) :: asmgen tail
	| BB.ArrayAlloc (t, n) -> Push (I (VA.Int_l (VA.sizeof t))) :: Push (R (TempR n)) :: Call (Id.L "_nibkame_array_alloc_") :: Add (RI (ESP, VA.Int_l 4)) :: Mov (TempR dst, EAX) :: asmgen tail
      end
	
  | BB.If (ins, b_label) :: tail ->
      begin match ins with
	| BB.Comp (op, ty, src1, src2) ->
	    begin match ty with
	      | VA.Int | VA.Char ->
		  (match src2 with
		     | VA.C 0 when op = VA.Eq -> Test (RI (TempR src1, VA.Int_l 0)) :: Branch (Zero, b_label) :: asmgen tail
		     | VA.C 0 when op = VA.NotEq -> Test (RI (TempR src1, VA.Int_l 0)) :: Branch (NotZero, b_label) :: asmgen tail
		     | _ -> Cmp (twoOp_to_twoOp src1 src2) :: Branch (to_cmp op, b_label) :: asmgen tail)
	      | VA.Float -> let VA.V s2 = src2 in FComp (TempF src1, TempF s2) :: Branch (to_cmp op, b_label) :: asmgen tail
	      | VA.Pointer t when op = VA.Eq ->
		  let func = match t with
		    | VA.Array _ -> Id.L "_nibkame_array_compare_"
		    | VA.List _ -> Id.L "_nibkame_list_compare_"
		    | VA.Tuple _ -> Id.L "_nibkame_tuple_comare_"
		    | VA.Undefined -> Id.L "_nibkame_generic_comare_"
		  in
		    Push (id_or_imm_to_rmi src2) :: Push (R (TempR src1)) :: Call (func) :: Add (RI (ESP, VA.Int_l 8))
		    :: Test (RR (EAX, EAX)) :: Branch (Zero, b_label) :: asmgen tail
	      | VA.Pointer t when op = VA.NotEq ->
		  let func = match t with
		    | VA.Array _ -> Id.L "_nibkame_array_compare_"
		    | VA.List _ -> Id.L "_nibkame_list_compare_"
		    | VA.Tuple _ -> Id.L "_nibkame_tuple_comare_"
		    | VA.Undefined -> Id.L "_nibkame_generic_comare_"
		  in
		    Push (id_or_imm_to_rmi src2) :: Push (R (TempR src1)) :: Call (func) :: Add (RI (ESP, VA.Int_l 8))
		    :: Test (RR (EAX, EAX)) :: Branch (NotZero, b_label) :: asmgen tail
	      | VA.Pointer _ | VA.Fun _ -> failwith "Not Supported Compare type."
	    end
	| ins -> 
	    let t = VA.temp () in
	      asmgen (BB.Let ((t, VA.Int), ins) :: BB.If ((BB.Comp (VA.NotEq, VA.Int, t, VA.C 0)), b_label) :: tail)
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

let generate_function { BB.name = name; BB.body = body; BB.block_labels = labels } =
  { name = name; body = asmgen body }

let output_function chan { name = Id.L label; body = body } =
  output_string chan (Format.sprintf ".global %s\n" label);
  List.iter (fun ins -> output_char chan '\n'; output_string chan (str_of_inst ins)) body;
  output_char chan '\n'
