let nextToken stream =
  let buff = Buffer.create 50 in
  let rec str stm =
    match Stream.peek stm with
      | Some ch ->
	  (match ch with
	     | ')' -> Buffer.contents buff
	     | ' ' | '\t' | '\n' ->
		 Stream.junk stm;
		 Buffer.contents buff
	     | _ ->
		 Stream.junk stm;
		 Buffer.add_char buff ch;
		 str stm)
      | None -> Buffer.contents buff in
    (* Buffer.reset buff; *)
  let rec iter stm =
    match Stream.peek stream with
      | Some ch ->
	  (match ch with
	     | '(' | ')' ->
		 Stream.junk stm;
		 String.make 1 ch
	     | ' ' | '\t' | '\n' ->
		 Stream.junk stream;
		 iter stm
	     | _ -> str stm)
      | None -> raise End_of_file
  in
    iter stream


let ending stm syn : Syntax.t =
  ignore (nextToken stm);
  syn

exception S_expr_is_end

(*
  read_expr : char Stream.t -> Syntax.t
*)
let rec read_expr stm =
  let reader _ =
    let rec list_reader func =
      try (func (read_expr stm)) :: list_reader func with
	| S_expr_is_end -> []
    in
      match nextToken stm with
	| ")" -> Syntax.Unit
	    
	| "list" -> Syntax.List (list_reader (fun x -> x))
	| "tuple" -> Syntax.Tuple (list_reader (fun x -> x))
	| "array" -> Syntax.Array (list_reader (fun x -> x))

	| "+" -> ending stm (Syntax.Add (read_expr stm, read_expr stm))
	| "-" -> ending stm (Syntax.Sub (read_expr stm, read_expr stm))
	| "*" -> ending stm (Syntax.Mul (read_expr stm, read_expr stm))
	| "/" -> ending stm (Syntax.Div (read_expr stm, read_expr stm))
	| "+." -> ending stm (Syntax.Fadd (read_expr stm, read_expr stm))
	| "-." -> ending stm (Syntax.Fsub (read_expr stm, read_expr stm))
	| "*." -> ending stm (Syntax.Fmul (read_expr stm, read_expr stm))
	| "/." -> ending stm (Syntax.Fdiv (read_expr stm, read_expr stm))
	    
	| "=" -> ending stm (Syntax.Eq (read_expr stm, read_expr stm))
	| "<>" -> ending stm (Syntax.NotEq (read_expr stm, read_expr stm))
	| "<=" -> ending stm (Syntax.LsEq (read_expr stm, read_expr stm))
	| "<" -> ending stm (Syntax.Ls (read_expr stm, read_expr stm))
	| ">" -> ending stm (Syntax.Gt (read_expr stm, read_expr stm))
	| ">=" -> ending stm (Syntax.GtEq (read_expr stm, read_expr stm))
	    
	| "cons" -> ending stm (Syntax.Cons (read_expr stm, read_expr stm))
	| ";" -> ending stm (Syntax.Seq (read_expr stm, read_expr stm))
	| "if" -> ending stm (Syntax.If (read_expr stm, read_expr stm,
					 read_expr stm))
	| "fun" -> 
	    let rec iter func =
	      match nextToken stm with
		| ")" -> []
		| str -> (func str) :: iter func
	    in
	      ending stm 
		(Syntax.Fun
		   ((match nextToken stm with
		       | "(" -> iter (fun x -> (x, Type.gentype ()))
		       | _ -> raise (Failure "unreconized string")),
		    read_expr stm))
	| "let" ->
	    ending stm (Syntax.Let ((nextToken stm, Type.gentype ()),
				    read_expr stm, read_expr stm))
	| "letrec" ->
	    ending stm (Syntax.LetRec ((nextToken stm, Type.gentype()),
				       read_expr stm, read_expr stm))
	| funname -> Syntax.Unit
  in
    match nextToken stm with
      | "(" ->
	  reader ()
      | ")" -> raise S_expr_is_end
      | "unit" -> Syntax.Unit
      | str ->
	  try Syntax.Int (int_of_string str) with
	    | Failure "int_of_string" ->
		try Syntax.Float (float_of_string str) with
		  | Failure "float_of_string" ->
		      (Syntax.Var str)
