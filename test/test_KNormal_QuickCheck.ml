open QuickCheck
open TestUtil
module TestType = Test_Type_QuickCheck

(* PShow instances *)
module PShow_KNormal = struct
  type t = KNormal.t
  let show : t -> pretty_str =
    fun s fmt () ->
        Sexpr.write fmt (KNormal.to_sexpr s)
end

(* Arbitrary instances *)
module Arbitrary_string = struct
  type t = string
  let arbitrary =
    choose_int (0x40, 0x7e) >>= fun a ->
        let str = String.create 1 in
        str.[0] <- Char.chr a;
        ret_gen str
end

module Arbitrary_string_list = Arbitrary_list(Arbitrary_string)

