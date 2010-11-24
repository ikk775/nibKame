open QuickCheck
open TestUtil
open Test_MyUtil_QuickCheck

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
