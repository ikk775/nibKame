module VA = VirtualAsm

type t =
  | Ans of exp
  | Seq of t * t
  | Let of (Id.t * VA.ty) * exp * t
and exp =
  | Nop
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
  | If of exp * t * t

  | ApplyCls of (Id.t * VA.ty) * Id.t list
  | ApplyDir of (Id.l * VA.ty) * Id.t list

  | Cons of Id.t * Id.t
  | Car of Id.t
  | Cdr of Id.t
  | FCons of Id.t * Id.t
  | FCar of Id.t
  | FCdr of Id.t

  | TupleAlloc of bool * (Id.t * VA.ty) list
  | ArrayAlloc of bool * VA.ty * Id.t

type fundef = { name: Id.l; args: (Id.t * VA.ty) list; body: t; ret: VA.ty }

let through = function
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

  | VA.ApplyCls (cls, args) -> ApplyCls (cls, args)
  | VA.ApplyDir (func, args) -> ApplyDir (func, args)

  | VA.Cons (h, t) -> Cons (h, t)
  | VA.Car (l) -> Car l
  | VA.Cdr (l) -> Cdr l
  | VA.FCons (h, t) -> FCons (h, t)
  | VA.FCar (l) -> FCar l
  | VA.FCdr (l) -> FCdr l

  | VA.TupleAlloc (data) -> TupleAlloc (true, data)
  | VA.ArrayAlloc (ty, num) -> ArrayAlloc (true, ty, num)


(* ソースオペランドに含むか調べる *)
let rec find id exp =
  let find' = function
    | VA.Nop -> false
    | VA.Set (lit) -> false
    | VA.Mov (VA.V v) -> id = v
    | VA.Mov (VA.C _) -> false
    | VA.Neg _ | VA.Add _ | VA.Sub _ | VA.Mul _ | VA.Div _ | VA.SLL _ | VA.SLR _ | VA.Ld _ | VA.St _ -> false
    | VA.FMov (v) -> id = v
    | VA.FNeg _ | VA.FAdd _ | VA.FSub _ | VA.FMul _ | VA.FDiv _ | VA.FLd _ | VA.FSt _ -> false

    | VA.BLd _ | VA.BSt _ | VA.Comp _ -> false
	
    | VA.ApplyCls (cls, args) -> List.fold_left (fun prev i -> prev || i = id) false args
    | VA.ApplyDir (func, args) -> List.fold_left (fun prev i -> prev || i = id) false args

    | VA.Cons (h, t) -> id = h
    | VA.Car _ | VA.Cdr _ -> false
    | VA.FCons (h, t) -> id = h
    | VA.FCar _ | VA.FCdr _ -> false

    | VA.TupleAlloc (data) -> List.fold_left (fun prev i -> prev || i = id) false (List.rev_map fst data)
    | VA.ArrayAlloc (ty, num) -> false
	
    | VA.If (exp, t, f) -> find id t || find id f
  in
    match exp with
      | VA.Let ((dst, _), VA.Mov (VA.V src), next) ->
	  if src = id 
	  then find id next || find dst next
	  else find id next
  | VA.Let ((dst, _), VA.FMov (src), next) ->
      if src = id 
      then find id next || find dst next
      else find id next
  | VA.Let (_, ins, next) ->
      find' ins || find id next
  | VA.Ans ins ->
      find' ins
  | VA.Seq (b, a) ->
      find id b || find id a

let rec g next = function
  | VA.Let ((id, ty), VA.TupleAlloc (l), exp) ->
      Let ((id, ty),
	   TupleAlloc (List.fold_left (fun p c -> p || find id c) (find id exp) next,
		       l),
	   g next exp)
  | VA.Let ((id, ty), VA.ArrayAlloc (typ, n), exp) ->
      Let ((id, ty),
	   ArrayAlloc (List.fold_left (fun p c -> p || find id c) (find id exp) next,
		       typ, n),
	   g next exp)
	
  (* 以下スルー *)
  | VA.Let (dst, ins, n) ->
      Let (dst, through ins, g next n)
  | VA.Ans ins ->
      Ans (through ins)
  | VA.Seq (b, a) ->
      Seq (g (a :: next) b, g next a)

let f' { VA.name = n; VA.args = a; VA.body = b; VA.ret = r } =
  {name = n; args = a; body = g [] b; ret = r}

let f l =
  List.map f' l
