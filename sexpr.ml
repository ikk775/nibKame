
type t =
  | Sstring of string
  | Sident of string
  | Sint of int
  | Sfloat of float
  | Schar of char
  | Sexpr of t list

let nextToken stream =
  let buff = Buffer.create 50 in
  let rec in_string stm =
    match Stream.peek stm with
      | Some '\\' ->
	  Stream.junk stm;
	  Buffer.add_char buff '\\';
	  Buffer.add_char buff (Stream.next stm);
	  in_string stm
      | Some '"' ->
	  Stream.junk stm;
	  Buffer.add_char buff '"';
	  Buffer.contents buff
      | Some c ->
	  Stream.junk stm;
	  Buffer.add_char buff c;
	  in_string stm
      | None -> Buffer.contents buff
  in
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
  let rec iter stm =
    match Stream.peek stream with
      | Some ch ->
	  (match ch with
	     | '(' | ')' ->
		 Stream.junk stm;
		 String.make 1 ch
	     | ' ' | '\t' | '\n' ->
		 Stream.junk stm;
		 iter stm
	     | '"' ->
		 Stream.junk stm;
		 Buffer.add_char buff '"';
		 in_string stm
	     | _ -> str stm)
      | None -> raise End_of_file
  in
    iter stream

type te =
  | D of string
  | L of te list
  | E

let rec make_te stm =
    match nextToken stm with
      | "(" -> L (make_list stm)
      | ")" -> E
      | str -> D (str)
and make_list stm =
  match make_te stm with
    | D a -> (D a) :: make_list stm
    | L a -> (L a) :: make_list stm
    | E -> []

let lexer = Genlex.make_lexer []

let rec build_tree = function
  | D a ->
      if String.get a 0 = '\'' 
	&& String.get a (String.length a - 1) <> '\'' then
	Sident a 
      else
	let s = lexer (Stream.of_string a) in
	  (match Stream.next s with
	     | Genlex.Ident i -> Sident i
	     | Genlex.Int i -> Sint i
	     | Genlex.Float f -> Sfloat f
	     | Genlex.Char c -> Schar c
	     | Genlex.String s -> Sstring s
		 (*  | Genlex.Kwd _ -> raise Not_found *))
  | L a -> Sexpr (List.map build_tree a)
  | E -> Sexpr []


let read stream =
  build_tree (make_te stream)

let rec write fmtr  = function
	| Sstring s -> Format.fprintf fmtr "%S" s
	| Schar c -> Format.fprintf fmtr "%C" c
	| Sident s -> Format.fprintf fmtr "%s" s
	| Sint i -> Format.fprintf fmtr "%d" i
	| Sfloat f -> Format.fprintf fmtr "%f" f
	| Sexpr l ->
			Format.fprintf fmtr "(";
			if l != []
			then
				write fmtr (List.hd l);
				if (List.tl l) != []
				then
					List.iter (Format.fprintf fmtr " "; write fmtr) (List.tl l);
			Format.fprintf fmtr ")"