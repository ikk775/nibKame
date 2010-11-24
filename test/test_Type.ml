open QuickCheck
open TestUtil
open Test_Type_QuickCheck

(* Tests *)

let prop_equal : 'a -> bool =
  fun x ->
    try
      gen_prop_equality Std.dump Type.equal x x
    with
      | _ -> false
let () = Check_type_to_bool.quickCheck prop_equal

let prop_equal2 : 'a -> bool =
  fun x ->
    try
      gen_prop_equality Std.dump (=) x x
    with
      | _ -> false
let () = Check_type_to_bool.quickCheck prop_equal2

let prop_sexpr_convert : 'a -> bool =
  fun x ->
    try
      let y = Type.of_sexpr(Type.to_sexpr x) in
      gen_prop_equality Std.dump Type.equal x y
    with
      | _ -> false
let () = Check_type_to_bool.quickCheck prop_sexpr_convert

let prop_string_convert : 'a -> bool =
  fun x ->
    try
      let s1 = Type.to_sexpr x in
      let s2 = Sexpr.from_string (Sexpr.to_string s1) in
      gen_prop_equality Std.dump Sexpr.equal s1 s2
    with
      | _ -> false
let () = Check_type_to_bool.quickCheck prop_string_convert

let prop_sexpr_string_convert : 'a -> bool =
  fun x ->
    try
      let s1 = Type.to_sexpr x in
      let s2 = Sexpr.from_string (Sexpr.to_string s1) in
      let y = Type.of_sexpr s2 in
      gen_prop_equality Std.dump Type.equal x y
    with
      | _ -> false
let () = Check_type_to_bool.quickCheck prop_sexpr_string_convert
