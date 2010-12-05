
type t =
  | Sstring of string
  | Sident of string
  | Sint of int
  | Sfloat of float
  | Schar of char
  | Sexpr of t list

let rec skipBrank stm =
  match Stream.peek stm with
    | Some ' '
    | Some '\t'
    | Some '\n' ->
      Stream.junk stm;
      skipBrank stm
    | Some _ -> ()
    | None -> ()

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
  let rec in_char stm =
    try
      match Stream.peek stm with
	| Some '\\' -> 
	    Stream.junk stm;
	    Buffer.add_char buff '\\';
	    Buffer.add_char buff (Stream.next stm);
	    str stm
	| Some _ -> str stm
	| None -> str stm
    with | Stream.Failure -> str stm
  in 
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
		 Buffer.add_char buff (Stream.next stm);
		 in_string stm
	     | '#' -> 
		 Stream.junk stm;
		 Buffer.add_char buff '#';
		 in_char stm
	    | _ -> str stm)
      | None -> raise End_of_file
  in
    iter stream

type te =
  | D of string
  | L of te list
  | E

(*
  かっこのネストを対応するリストへ読み込み
*)
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
  文字列リテラル(文字配列)のエスケープ文字の変換
  第1引数には [] を渡すこと
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
	 | _ as c -> invalid_arg (Format.sprintf "unreconized escape sequence %c" c))
  | c :: tail -> unescape (c :: l) tail
  | [] -> l

let charname_to_char = function
  | "space" -> ' '
  | "newline" | "nl" | "lf"  -> '\n'
  | "return" | "cr" -> '\r'
  | "tab" | "ht" -> '\t'
  | "page" -> '\012'
  | "escape" | "esc" -> '\028'
  | "delete" | "del" -> '\127'
  | "null" -> '\000'
  | "backspace" | "bs" -> '\b'
  | _ -> invalid_arg "unrconized char name"

let char_to_charname = function
  | ' ' -> "space"
  | '\n' -> "newline"
  | '\r' -> "return"
  | '\t' -> "tab"
  | '\012' -> "page"
  | '\028' -> "escape"
  | '\127' -> "delete"
  | '\000' -> "null"
  | '\b' -> "backspace"
  | c -> MyUtil.String.implode [c]

let isdigit = function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-'
      -> true
  | _ -> false

let rec build_tree = function
  | D a ->
      let l = MyUtil.String.explode a in
	(match List.hd l with     
	   | '"' -> Sstring (MyUtil.String.implode (List.rev (List.tl (unescape [] (List.tl l)))))
	   | '#' ->
	        (match List.tl l with
		  | '\\' :: c :: [] -> Schar c
		  | '\\' :: tail ->
		      if List.hd tail = 'x' then
			Schar (char_of_int (int_of_string (MyUtil.String.implode ('0' ::  tail))))
		      else
			(match MyUtil.String.implode (List.map Char.lowercase tail) with
			   | "space" -> Schar ' '
			   | "newline" | "nl" | "lf"  -> Schar '\n'
			   | "return" | "cr" -> Schar '\r'
			   | "tab" | "ht" -> Schar '\t'
			   | "page" -> Schar '\012'
			   | "escape" | "esc" -> Schar '\028'
			   | "delete" | "del" -> Schar '\127'
			   | "null" -> Schar '\000'
			   | "backspace" | "bs" -> Schar '\b'
			   | _ -> invalid_arg "unrconized char lietral")
		  | _ -> Sident a)
	   | c when isdigit c -> 
	       (try Sint (int_of_string a) with
		 | Failure _  -> (try Sfloat (float_of_string a) with
		     | _ -> Sident a)
		 | _ -> Sident a)
	   | _ -> Sident a)
  | L a -> Sexpr (List.map build_tree a)
  | E -> Sexpr []

let read stream =
  skipBrank stream;
  match Stream.peek stream with
    | Some _ -> build_tree (make_te stream)
    | None -> raise Stream.Failure

let rec equal x y =
  match x, y with
  | Sstring x, Sstring y when x = y -> true
  | Sident x, Sident y when x = y -> true
  | Sint x, Sint y when x = y -> true
  | Schar x, Schar y when x = y -> true
  | Sfloat x, Sfloat y when x = y -> true
  | Sexpr x, Sexpr y -> 
    List.for_all2 equal x y
  | _ -> false

let rec write fmtr  = function
  | Sstring s -> Format.fprintf fmtr "%S" s
  | Sident s -> Format.fprintf fmtr "%s" s
  | Schar c -> Format.fprintf fmtr "#\\%s" (char_to_charname c)
  | Sint i -> Format.fprintf fmtr "%s" (string_of_int i)
  | Sfloat f -> Format.fprintf fmtr "%.30f"  f
  | Sexpr l ->
      Format.fprintf fmtr "@[<hov 2>(";
      (match l with
        | [] -> ()
        | e :: es ->
          write fmtr e;
		  List.iter (fun e -> Format.fprintf fmtr "@ "; write fmtr e) es);
      Format.fprintf fmtr ")@]@,"

let to_string x =
  MyUtil.Format.call_with_output_string (fun fmtr -> write fmtr x)

let from_string str =
  read (Stream.of_string str)
