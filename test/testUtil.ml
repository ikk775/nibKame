let call_with_output_string proc =
	let buf = Buffer.create 127 in
	let buf_f = Format.formatter_of_buffer buf in
	proc buf_f;
	Format.pp_print_flush buf_f ();
	Buffer.contents buf

let rec iota ?(step=1) s e =
  if s > e then []
  else s :: iota ~step (s + step) e

let rec setDiff xs ys =
	List.filter (fun x -> not (List.mem x ys)) xs