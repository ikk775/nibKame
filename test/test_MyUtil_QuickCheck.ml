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

let prop_List_shuffle_quanta : 'a -> bool = fun x ->
  let y = MyUtil.List.shuffle x in
  (List.length x) = (List.length y)
  
let prop_List_shuffle_constraint1 : 'a -> bool = fun x ->
  let y = MyUtil.List.shuffle x in
  List.for_all (fun z -> (List.mem z x)) y
let prop_List_shuffle_constraint2 : 'a -> bool = fun x ->
  let y = MyUtil.List.shuffle x in
  List.for_all (fun z -> (List.mem z y)) x
  
let () = Check_fun_string_list_to_bool.quickCheck prop_List_shuffle_quanta
let () = Check_fun_string_list_to_bool.quickCheck prop_List_shuffle_constraint1
let () = Check_fun_string_list_to_bool.quickCheck prop_List_shuffle_constraint2
  
let prop_setDiff_id : 'a -> bool = fun x -> 
  gen_prop_equality Std.dump (=) (MyUtil.List.setDiff x []) x
  
let prop_setDiff_nothing1 : 'a -> bool = fun x -> 
  gen_prop_equality Std.dump (=) (MyUtil.List.setDiff [] x) []
let prop_setDiff_nothing2 : 'a -> bool = fun x -> 
  gen_prop_equality Std.dump (=) (MyUtil.List.setDiff x x) []
  
let prop_setDiff_constraint1 : 'a -> bool = function
  |  x, y -> List.for_all (fun z -> not (List.mem z y)) (MyUtil.List.setDiff x y)
let prop_setDiff_constraint2 : 'a -> bool = function
  |  x, y -> List.for_all (fun z -> not (List.mem z (MyUtil.List.setDiff x y))) y
let prop_setDiff_constraint3 : 'a -> bool = function
  |  x, y -> List.for_all (fun z -> (List.mem z x)) (MyUtil.List.setDiff x y)
let prop_setDiff_constraint4 : 'a -> property = function
  |  x, y ->
    Implies_bool.(==>) (* FIXME *)
      (List.for_all (fun z -> (List.mem z x)) y)
      (List.for_all (fun z -> (List.mem z x)) (MyUtil.List.setDiff x y))

let () = Check_fun_string_list_to_bool.quickCheck prop_setDiff_id
let () = Check_fun_string_list_to_bool.quickCheck prop_setDiff_nothing1
let () = Check_fun_string_list_to_bool.quickCheck prop_setDiff_nothing2
let () = Check_fun_string_list_'_string_list_to_bool.quickCheck prop_setDiff_constraint1
let () = Check_fun_string_list_'_string_list_to_bool.quickCheck prop_setDiff_constraint2
let () = Check_fun_string_list_'_string_list_to_bool.quickCheck prop_setDiff_constraint3
let () = Check_fun_string_list_'_string_list_to_property.quickCheck prop_setDiff_constraint4

let prop_implode : 'a -> bool = fun x -> 
  gen_prop_equality Std.dump (=) (MyUtil.String.implode x) (ExtString.String.implode x)
  
let prop_explode : 'a -> bool = fun x -> 
  gen_prop_equality Std.dump (=) (MyUtil.String.explode x) (ExtString.String.explode x)
  
let prop_implode_explode : 'a -> bool = fun x -> 
  gen_prop_equality Std.dump (=) (MyUtil.String.implode (MyUtil.String.explode x)) x
  
let prop_explode_implode : 'a -> bool = fun x -> 
  gen_prop_equality Std.dump (=) (MyUtil.String.explode (MyUtil.String.implode x)) x
  

let () = Check_fun_char_list_to_bool.quickCheck prop_implode
let () = Check_fun_string_to_bool.quickCheck prop_explode
let () = Check_fun_char_list_to_bool.quickCheck prop_explode_implode
let () = Check_fun_string_to_bool.quickCheck prop_implode_explode
