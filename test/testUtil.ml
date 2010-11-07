open QuickCheck

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

module type ARBITRARY_STRING = sig
  type t = string
  val arbitrary : t gen
end

module Arbitrary_ascii_char = struct
  type t = char
  let arbitrary =
    choose_int (0x20, 0x7e) >>= fun c ->
        ret_gen (Char.chr c)
end
module Arbitrary_ascii_char_list = Arbitrary_list(Arbitrary_ascii_char)
module Arbitrary_string = struct
  type t = string
  let arbitrary =
    Arbitrary_ascii_char_list.arbitrary >>= lift_gen ExtString.String.implode
end
