open QuickCheck

module PShow_string = struct
  type t = string
  let show : t -> pretty_str =
    fun s fmt () ->
        Format.fprintf fmt "%S" s
end

module PShow_pair(Fst: PSHOW)(Snd: PSHOW) = struct
  type t = Fst.t * Snd.t
  let show : t -> pretty_str =
    fun p fmt () ->
      let x, y = p in
      let a1, a2 = Fst.show x, Snd.show y in
        Format.fprintf fmt "(%a, %a)" a1 () a2 ()
end

module PShow_Sexpr = struct
  type t = Sexpr.t
  let show : t -> pretty_str =
    fun s fmt () ->
        Sexpr.write fmt s
end

module type ARBITRARY_CHAR_LIST = sig
  type t = char list
  val arbitrary : t gen
end

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

let gen_prop_equality to_string eq x y =
  if (eq x y)
  then
    true
  else
    (Format.printf "Mismatching: @,@[%s@ != %s@]\n@?"
        (to_string x)
        (to_string y);
      false)

module Implies_bool = Implies(Testable_bool)

module Testable_int_to_bool =
  Testable_fun
  (Arbitrary_int)
  (PShow_int)
  (Testable_bool) ;;
module Check_int_to_bool = Check(Testable_int_to_bool)

let logsized f = sized f >>= (lift_gen (fun i -> (truncate (log (float_of_int i)))))
