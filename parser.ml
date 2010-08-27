let accept stm buf =
  Buffer.add_char buf (Stream.next stm)

let digit stm =
  match Stream.peek stm with
    | Some c ->
	if isdigit c then
	  Some (String.make 1 c)
	else
	  None
    | None -> None

let isdigit c =
  true

let make_one_char ischar stm =
  match Stream.peek stm with
    | Some c ->
	if ischar c then
	  (Stream.junk stm; Some (String.make 1 c))
	else
	  None
    | None -> None

let space =
  make_one_char
    (function
       | ' ' -> true
       | '\t' -> true
       | '\n' -> true
       | '\r' -> true
       | _ -> false)

let zero_or_more onechar =
  let buf = Buffer.create 16 in
  let rec func stm =
    match onechar stm with
      | Some str ->
	(Buffer.add_string buf str; func stm)
      | None ->
	let str = Buffer.contents buf in
	  Buffer.reset buf;
	  Some str
  in
    func

let spaces = zero_or_more space
