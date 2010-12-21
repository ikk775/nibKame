open QuickCheck
open TestUtil
open Test_Typing_QuickCheck

(* Tests *)
let prop_sexpr_convert : 'a -> bool =
  fun x ->
    try
      let y = Typing.of_sexpr(Typing.to_sexpr x) in
      gen_prop_equality Std.dump (=) x y
    with
      | _ -> false
let () = Check_fun_result_to_bool.quickCheck prop_sexpr_convert

let prop_string_convert : 'a -> bool =
  fun x ->
    try
      let s1 = Typing.to_sexpr x in
      let s2 = Sexpr.from_string (Sexpr.to_string s1) in
      gen_prop_equality Std.dump Sexpr.equal s1 s2
    with
      | _ -> false
let () = Check_fun_result_to_bool.quickCheck prop_string_convert

let prop_sexpr_string_convert : 'a -> bool =
  fun x ->
    try
      let s1 = Typing.to_sexpr x in
      let s2 = Sexpr.from_string (Sexpr.to_string s1) in
      let y = Typing.of_sexpr s2 in
      gen_prop_equality Std.dump (=) x y
    with
      | _ -> false
let () = Check_fun_result_to_bool.quickCheck prop_sexpr_string_convert
