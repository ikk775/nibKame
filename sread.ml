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


(*
  read_expr : char Stream.t -> Syntax.t
*)
let rec read_expr stream =
  match nextToken stream with
    | "(" ->
	Syntax.Unit
    | "unit" -> Syntax.Unit
    | str ->
	try Syntax.Int (int_of_string str) with
	  | Failure "int_of_string" ->
	      try Syntax.Float (float_of_string str) with
		| Failure "float_of_string" ->
		    (Syntax.Var str)
