open MyUtil

let escape str =
  let f = function
    | '_' -> "_ul"
    | '%' -> "_pc"
    | '$' -> "_dr"
    | '.' -> "_dt"
    | ',' -> "_cm"
    | ':' -> "_cl"
    | ';' -> "_sc"
    | ' ' -> "_sp"
    | '!' -> "_ex"
    | '?' -> "_qu"
    | '#' -> "_hs"
    | '\'' -> "_qt"
    | '`' -> "_qq"
    | '"' -> "_qw"
    | '|' -> "_or"
    | '&' -> "_et"
    | '@' -> "_at"
    | '^' -> "_ac"
    | '+' -> "_pl"
    | '-' -> "_mn"
    | '*' -> "_as"
    | '~' -> "_tl"
    | '/' -> "_sl"
    | '\\' -> "_bs"
    | '=' -> "_eq"
    | '(' -> "_ps"
    | ')' -> "_pf"
    | '{' -> "_bs"
    | '}' -> "_bf"
    | '[' -> "_ss"
    | ']' -> "_sf"
    | '<' -> "_lt"
    | '>' -> "_gt"
    | x -> String.implode [x] in
  List.fold_left (^) "" (List.map f (String.explode str))

let escapex prefix str =
  if str.[0] = '_'
  then prefix ^ str
  else escape str

let unescape stm =
  let rec f cs =
    match Stream.next stm with
      | '_' -> begin
	match String.implode (Stream.npeek 2 stm) with
	  | "ul" -> f ('_' :: cs)
	  | "pc" -> f ('%' :: cs)
	  | "dr" -> f ('$' :: cs)
	  | "dt" -> f ('.' :: cs)
	  | "cm" -> f (',' :: cs)
	  | "cl" -> f (':' :: cs)
	  | "sc" -> f (';' :: cs)
	  | "sp" -> f (' ' :: cs)
	  | "ex" -> f ('!' :: cs)
	  | "qu" -> f ('?' :: cs)
	  | "hs" -> f ('#' :: cs)
	  | "qt" -> f ('\'' :: cs)
	  | "qq" -> f ('`' :: cs)
	  | "qw" -> f ('"' :: cs)
	  | "or" -> f ('|' :: cs)
	  | "et" -> f ('&' :: cs)
	  | "at" -> f ('@' :: cs)
	  | "ac" -> f ('^' :: cs)
	  | "pl" -> f ('+' :: cs)
	  | "mn" -> f ('-' :: cs)
	  | "as" -> f ('*' :: cs)
	  | "tl" -> f ('~' :: cs)
	  | "sl" -> f ('/' :: cs)
	  | "bs" -> f ('\\' :: cs)
	  | "eq" -> f ('=' :: cs)
	  | "ps" -> f ('(' :: cs)
	  | "pf" -> f (')' :: cs)
	  | "bs" -> f ('{' :: cs)
	  | "bf" -> f ('}' :: cs)
	  | "ss" -> f ('[' :: cs)
	  | "sf" -> f (']' :: cs)
	  | "lt" -> f ('<' :: cs)
	  | "gt" -> f ('>' :: cs)
	  | _ -> failwith "undefined underline escape"
      end

      | c -> f (c :: cs)
  in
  String.implode (List.rev (f []))
let write_number n =
  Format.sprintf "%ds" n

let read_number stm =
  let rec f cs =
    match Stream.next stm with
      | c when c = 's' -> cs
      | c when String.contains "0123456789" c -> f (c :: cs)
      | c -> invalid_arg "of_string"
  in
  int_of_string (String.implode (List.rev (f [])))

let write_id x =
  assert (Id.is_valid x);
  let x' = escape x in
  let len = String.length x' in
  Format.sprintf "%ds%s" len x'

let read_id stm =
  let cn = read_number stm in
  let rec f n cs =
    if n <= 0
    then cs
    else f (n - 1) (Stream.next stm :: cs)
  in
  unescape (Stream.of_string (String.implode (List.rev (f cn []))))

let write_seq f xs =
  let len = List.length xs in
  let strs = List.map f xs in
  let str = String.concat "" strs in
  Printf.sprintf "%ds" len ^ str

let read_seq stm f =
  let xn = read_number stm in
  List.iter_list xn (fun () -> f stm)
