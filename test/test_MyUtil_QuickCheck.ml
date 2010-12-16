open QuickCheck
open TestUtil

module PShow_string = struct
  type t = string
  let show : t -> pretty_str =
    fun s fmt () ->
        Format.fprintf fmt "%S" s
end
module PShow_char_list = PShow_list(PShow_char) ;;
module PShow_string_list = PShow_list(PShow_string) ;;
module PShow_string_'_string = PShow_pair(PShow_string)(PShow_string) ;;
module PShow_string_list_'_string = PShow_pair(PShow_string_list)(PShow_string) ;;
module PShow_string_list_'_string_list = PShow_pair(PShow_string_list)(PShow_string_list) ;;

module Arbitrary_char_list = Arbitrary_list(Arbitrary_ascii_char)
module Arbitrary_short_list(Elt:ARBITRARY) = struct
  type t = Elt.t list
  let arbitrary =
    choose_int(1, 2) >>= vector Elt.arbitrary
end
module Arbitrary_short_char_list = Arbitrary_short_list(Arbitrary_ascii_char)
module Arbitrary_list_to_string(Elt:ARBITRARY_CHAR_LIST) = struct
  type t = string
  let arbitrary =
    Elt.arbitrary >>= lift_gen ExtString.String.implode
end
module Arbitrary_string = Arbitrary_list_to_string(Arbitrary_short_char_list)
module Arbitrary_string_list = Arbitrary_list(Arbitrary_string)
module Arbitrary_string_'_string = Arbitrary_pair(Arbitrary_string)(Arbitrary_string)
module Arbitrary_string_list_'_string_list = Arbitrary_pair(Arbitrary_string_list)(Arbitrary_string_list)
module Arbitrary_string_list_'_string = Arbitrary_pair(Arbitrary_string_list)(Arbitrary_string)

module Testable_string_to_bool =
  Testable_fun
  (Arbitrary_string)
  (PShow_string)
  (Testable_bool) ;;
module Check_fun_string_to_bool = Check(Testable_string_to_bool)

module Testable_fun_char_list_to_bool =
  Testable_fun
  (Arbitrary_char_list)
  (PShow_char_list)
  (Testable_bool) ;;
module Check_fun_char_list_to_bool = Check(Testable_fun_char_list_to_bool)

module Testable_fun_string_list_to_bool =
  Testable_fun
  (Arbitrary_string_list)
  (PShow_string_list)
  (Testable_bool) ;;
module Check_fun_string_list_to_bool = Check(Testable_fun_string_list_to_bool)

module Testable_fun_string_'_string_to_bool =
  Testable_fun
  (Arbitrary_string_'_string)
  (PShow_string_'_string)
  (Testable_bool) ;;
module Check_fun_string_'_string_to_bool = Check(Testable_fun_string_'_string_to_bool)

module Testable_fun_string_list_'_string_list_to_bool =
  Testable_fun
  (Arbitrary_string_list_'_string_list)
  (PShow_string_list_'_string_list)
  (Testable_bool) ;;
module Check_fun_string_list_'_string_list_to_bool = Check(Testable_fun_string_list_'_string_list_to_bool)

module Testable_fun_string_list_'_string_list_to_property =
  Testable_fun
  (Arbitrary_string_list_'_string_list)
  (PShow_string_list_'_string_list)
  (Testable_property) ;;
module Check_fun_string_list_'_string_list_to_property = Check(Testable_fun_string_list_'_string_list_to_property)

module Testable_fun_string_list_'_string_to_bool =
  Testable_fun
  (Arbitrary_string_list_'_string)
  (PShow_string_list_'_string)
  (Testable_bool) ;;
module Check_fun_string_list_'_string_to_bool = Check(Testable_fun_string_list_'_string_to_bool)
