open QuickCheck
open TestUtil
open Test_TypingType_QuickCheck

module TT = TypingType
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
    let vs = TypingType.gen_typevars n in
    List.length vs == List.length (ExtList.List.unique vs)
let () = Check_int_to_bool.quickCheck prop_genTypeVars_duplication
    
let prop_typeVars_duplication : 'a -> bool =
  fun xs ->
      let vs = TypingType.typevars xs in
      List.length vs == List.length (ExtList.List.unique vs)
let () = Check_oType_to_bool.quickCheck prop_typeVars_duplication

let prop_freeTypeVars_duplication : 'a -> bool =
  fun xs ->
      let vs = TypingType.freetypevars xs in
      List.length vs == List.length (ExtList.List.unique vs)
let () = Check_typeScheme_to_bool.quickCheck prop_freeTypeVars_duplication

let prop_self_unify : 'a -> bool =
  fun x ->
    try
      [] == TypingType.unify x x
    with
      | _ -> false
let () = Check_oType_to_bool.quickCheck prop_self_unify

let prop_unify_var : 'a -> bool =
  fun x ->
    try
      ignore (TypingType.unify (TypingType.gen_typevar ()) x); true
    with
      | _ -> false
let () = Check_oType_to_bool.quickCheck prop_self_unify

let prop_unify_var : 'a -> bool =
  let rec walk = function
    | TT.O_Constant _ -> TT.gen_typevar ()
    | TT.O_Variable x -> TT.gen_typevar ()
    | TT.O_Ref t -> TT.O_Ref (walk t)
    | TT.O_Fun (t1, t2) -> TT.O_Fun (walk t1, walk t2)
    | TT.O_Variant (t1, t2) -> TT.O_Variant (walk t1, walk t2)
    | TT.O_Vector t -> TT.O_Vector (walk t)
    | TT.O_Tuple ts -> TT.O_Tuple (List.map walk ts)
  in
  fun x ->
    try
      ignore (TypingType.unify x (walk x)); true
    with
      | _ -> false
let () = Check_oType_to_bool.quickCheck prop_self_unify