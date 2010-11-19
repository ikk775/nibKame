open QuickCheck
open TestUtil
module TestType = Test_Type_QuickCheck

(* PShow instances *)
module PShow_oType = struct
  type t = TypingType.oType
  let show : t -> pretty_str =
    fun s fmt () ->
        Sexpr.write fmt (TypingType.oType_to_sexpr s)
end
module PShow_typeScheme = struct
  type t = TypingType.typeScheme
  let show : t -> pretty_str =
    fun s fmt () ->
        Sexpr.write fmt (TypingType.typeScheme_to_sexpr s)
end
module PShow_string_list = PShow_list(PShow_string)

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

module Arbitrary_oType = struct
  type t = TypingType.oType
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_string.arbitrary >>= fun s ->
        TestType.Arbitrary_type.arbitrary >>= fun t -> 
          oneof [
            ret_gen (TypingType.O_Constant t);
            ret_gen (TypingType.O_Variable s);
            ]
      else
        arb_sub (depth - 1) >>= fun e1 ->
        arb_sub (depth - 1) >>= fun e2 ->
        sized choose_int0 >>= vector (arb_sub (depth - 1)) >>= fun es -> 
          oneof[
            arb_sub 0;
            ret_gen (TypingType.O_Tuple(es));
            ret_gen (TypingType.O_Vector(e1));
            ret_gen (TypingType.O_Variant(e1, e2));
            ret_gen (TypingType.O_Fun(e1, e2));
            ]
    in
    sized (fun i ->
        arb_sub (truncate (log (float_of_int i))))
end

module Arbitrary_typeScheme = struct
  type t = TypingType.typeScheme
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_oType.arbitrary >>= fun t ->
            ret_gen (TypingType.OType(t));
      else
        Arbitrary_string_list.arbitrary >>= fun vs ->
            arb_sub (depth - 1) >>= fun qt ->
                oneof[
                  arb_sub 0;
                  ret_gen (TypingType.QType(ExtList.List.unique vs, qt))
                  ]
    in
    sized (fun i ->
        arb_sub (truncate (log (float_of_int i))))
end

(* Testables instances *)
module Testable_typeScheme_to_bool =
  Testable_fun
  (Arbitrary_typeScheme)
  (PShow_typeScheme)
  (Testable_bool) ;;
module Check_typeScheme_to_bool = Check(Testable_typeScheme_to_bool)

module Arbitrary_typeScheme_list = Arbitrary_list(Arbitrary_typeScheme) ;;
module PShow_typeScheme_list = PShow_list(PShow_typeScheme) ;;
module Testable_typeScheme_list_to_bool =
  Testable_fun
  (Arbitrary_typeScheme_list)
  (PShow_typeScheme_list)
  (Testable_bool) ;;
module Check_typeScheme_list_to_bool = Check(Testable_typeScheme_list_to_bool)

module Testable_oType_to_bool =
  Testable_fun
  (Arbitrary_oType)
  (PShow_oType)
  (Testable_bool) ;;
module Check_oType_to_bool = Check(Testable_oType_to_bool)

module Arbitrary_oType_list = Arbitrary_list(Arbitrary_oType) ;;
module PShow_oType_list = PShow_list(PShow_oType) ;;
module Testable_oType_list_to_bool =
  Testable_fun
  (Arbitrary_oType_list)
  (PShow_oType_list)
  (Testable_bool) ;;
module Check_oType_list_to_bool = Check(Testable_oType_list_to_bool)

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
