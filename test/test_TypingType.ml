open QuickCheck
open TestUtil
open Test_TypingType_QuickCheck

(* Tests *)

let prop_sexpr_convert : 'a -> bool =
  fun x ->
    try
      let y = TypingType.typeScheme_of_sexpr(TypingType.typeScheme_to_sexpr x) in
      gen_prop_equality Std.dump (=) x y
    with
      | _ -> false
let () = Check_typeScheme_to_bool.quickCheck prop_sexpr_convert

let prop_string_convert : 'a -> bool =
  fun x ->
    try
      let s1 = TypingType.typeScheme_to_sexpr x in
      let s2 = Sexpr.from_string (Sexpr.to_string s1) in
      gen_prop_equality Std.dump Sexpr.equal s1 s2
    with
      | _ -> false
let () = Check_typeScheme_to_bool.quickCheck prop_string_convert

let prop_sexpr_string_convert : 'a -> bool =
  fun x ->
    try
      let s1 = TypingType.typeScheme_to_sexpr x in
      let s2 = Sexpr.from_string (Sexpr.to_string s1) in
      let y = TypingType.typeScheme_of_sexpr s2 in
      gen_prop_equality Std.dump (=) x y
    with
      | _ -> false
let () = Check_typeScheme_to_bool.quickCheck prop_sexpr_string_convert

let prop_genTypeVars_duplication =
  fun n -> 
    let vs = TypingType.genTypeVars n in
    List.length vs == List.length (ExtList.List.unique vs)
let () = Check_int_to_bool.quickCheck prop_genTypeVars_duplication
    
let prop_typeVars_duplication : 'a -> bool =
  fun xs ->
      let vs = TypingType.typeVars xs in
      List.length vs == List.length (ExtList.List.unique vs)
let () = Check_oType_to_bool.quickCheck prop_typeVars_duplication

let prop_freeTypeVars_duplication : 'a -> bool =
  fun xs ->
      let vs = TypingType.freeTypeVars xs in
      List.length vs == List.length (ExtList.List.unique vs)
let () = Check_typeScheme_to_bool.quickCheck prop_freeTypeVars_duplication