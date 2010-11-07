
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

let rec eq_Sexpr x y =
  match x, y with
  | Sstring x, Sstring y when x = y -> true
  | Sident x, Sident y when x = y -> true
  | Sint x, Sint y when x = y -> true
  | Schar x, Schar y when x = y -> true
  | Sfloat x, Sfloat y when x = y -> true
  | Sexpr x, Sexpr y -> 
    List.for_all2 eq_Sexpr x y
  | _ -> false

let rec write fmtr  = function
  | Sstring s -> Format.fprintf fmtr "%S" s
  | Sident s -> Format.fprintf fmtr "%s" s
  | Schar c -> Format.fprintf fmtr "%C" c
  | Sint i -> Format.fprintf fmtr "%s" (string_of_int i)
  | Sfloat f -> Format.fprintf fmtr "%.30f"  f
  | Sexpr l ->
      Format.fprintf fmtr "@[(";
      (try
      write fmtr (List.hd l);
      List.iter (fun e -> Format.fprintf fmtr "@ "; write fmtr e) (List.tl l)
      with
        | Failure "hd" -> ()
        | Failure "tl" -> ());
      Format.fprintf fmtr ")@]@,"

let of_string x =
  TestUtil.call_with_output_string (fun fmtr -> write fmtr x)
