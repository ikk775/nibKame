open QuickCheck
open TestUtil

(* PShow instances *)
module PShow_string = struct
  type t = string
  let show : t -> pretty_str =
    fun s fmt () ->
        Format.fprintf fmt "%S" s
end
