
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
      | Some '"' | Some '\'' ->
	  Buffer.add_char buff (Stream.next stm);
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
	     | '"' | '\'' ->
		 Buffer.add_char buff (Stream.next stm);
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

(* 
let char_list_of_string str =
  let len = String.length str in
  let rec iter c =
    if c < len then str.[c] :: iter (c + 1)
    else [] in
    iter 0

let string_of_char_list list =
  let len = List.length list in
  let str = String.create len in
  let rec iter c = function
    | x :: xs -> String.set str c x; iter (c + 1) xs
    | [] -> str
  in
    iter 0 list

 Using ExtString.String.explode and implode
*)

let rec unescape l = function
  | '\\' :: c :: tail ->
      (match c with
	 | 't' -> unescape ('\t' :: l) tail
	 | 'n' -> unescape ('\n' :: l) tail
	 | 'r' -> unescape ('\r' :: l) tail
	 | 'b' -> unescape ('\b' :: l) tail
	 | '\'' -> unescape ('\'' :: l) tail
	 | '"' -> unescape ('"' :: l) tail
	 | '\\' -> unescape ('\\' :: l) tail
	 | _ -> invalid_arg "unreconized escape sequence")
  | c :: tail -> unescape (c :: l) tail
  | [] -> l

let isdigit = function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-'
      -> true
  | _ -> false

let rec build_tree = function
  | D a ->
      let l = ExtString.String.explode a in
	(match List.hd l with     
	   | '"' -> Sstring (ExtString.String.implode (List.rev (List.tl (unescape [] (List.tl l)))))
	   | '\'' ->
	        (match unescape [] (List.tl l) with
		  | '\'' :: c :: [] -> Schar c
		  | _ ->  invalid_arg "that char was too long")
	   | c when isdigit c -> 
	       (try Sint (int_of_string a) with
		 | Failure _  -> (try Sfloat (float_of_string a) with
		     | _ -> invalid_arg "invalid number 1")
		 | _ -> invalid_arg "invalid number 2")
	   | _ -> Sident a)
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
