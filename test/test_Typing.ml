open QuickCheck
open TestUtil

(* PShow instances *)
module PShow_string = struct
  type t = string
  let show : t -> pretty_str =
    fun s fmt () ->
        Format.fprintf fmt "%S" s
end
module PShow_expr = struct
  type t = Typing.expr
  let show : t -> pretty_str =
    fun s fmt () ->
        let rec f: t -> string = function
          | Typing.E_Constant s -> Format.sprintf "E_Constant(%S)" s
          | Typing.E_Variable s -> Format.sprintf "E_Variable(%S)" s
          | Typing.E_Fun(s, t) -> Format.sprintf "E_Fun(%S, %s)" s (f t)
          | Typing.E_Apply(t1, t2) -> Format.sprintf "E_Apply(%s, %s)" (f t1) (f t2)
          | Typing.E_Let(s, t1, t2) -> Format.sprintf "E_Let(%S, %s, %s)" s (f t1) (f t2)
        in
        Format.fprintf fmt "%s" (f s)
end
module PShow_oType = struct
  type t = Typing.oType
  let show : t -> pretty_str =
    fun s fmt () ->
        let rec f: t -> string = function
          | Typing.O_Constant -> Format.sprintf "O_Constant"
          | Typing.O_Variable s -> Format.sprintf "O_Variable(%S)" s
          | Typing.O_Fun(t1, t2) -> Format.sprintf "O_Fun(%s, %s)" (f t1) (f t2)
        in
        Format.fprintf fmt "%s" (f s)
end
module PShow_string_list = PShow_list(PShow_string)
module PShow_typeScheme = struct
  type t = Typing.typeScheme
  let show : t -> pretty_str =
    fun s fmt () ->
        let rec f: t -> string = function
          | Typing.OType t -> Format.sprintf "OType(%s)" (call_with_output_string (fun ff -> PShow_oType.show t ff ()))
          | Typing.QType(vs, ts) -> Format.sprintf "QType(%s, %s)" (call_with_output_string (fun ff -> PShow_string_list.show vs ff ())) (f ts)
        in
        Format.fprintf fmt "%s" (f s)
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

module Arbitrary_expr = struct
  type t = Typing.expr
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_string.arbitrary >>= fun s ->
            oneof[
              ret_gen (Typing.E_Constant s);
              ret_gen (Typing.E_Variable s)
              ]
      else
        Arbitrary_string.arbitrary >>= fun s ->
            arb_sub (depth - 1) >>= fun e ->
                arb_sub (depth - 1) >>= fun e1 ->
                    arb_sub (depth - 1) >>= fun e2 ->
                        oneof[
                          ret_gen (Typing.E_Constant s);
                          ret_gen (Typing.E_Variable s);
                          ret_gen (Typing.E_Fun(s, e));
                          ret_gen (Typing.E_Apply(e1, e2));
                          ret_gen (Typing.E_Let(s, e1, e2))
                          ]
    in
    choose_int(0, 3) >>= fun i ->
        arb_sub i
end

module Arbitrary_string_list = Arbitrary_list(Arbitrary_string)

module Arbitrary_oType = struct
  type t = Typing.oType
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_string.arbitrary >>= fun s ->
            oneof [
              ret_gen Typing.O_Constant;
              ret_gen (Typing.O_Variable s);
              ]
      else
        Arbitrary_string.arbitrary >>= fun s ->
            arb_sub (depth - 1) >>= fun e1 ->
                arb_sub (depth - 1) >>= fun e2 ->
                    oneof[
                      ret_gen Typing.O_Constant;
                      ret_gen (Typing.O_Variable s);
                      ret_gen (Typing.O_Fun(e1, e2))
                      ]
    in
    choose_int(0, 3) >>= fun i ->
        arb_sub i
end

module Arbitrary_typeScheme = struct
  type t = Typing.typeScheme
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_oType.arbitrary >>= fun t ->
            ret_gen (Typing.OType(t));
      else
        Arbitrary_oType.arbitrary >>= fun ot ->
            Arbitrary_string_list.arbitrary >>= fun vs ->
                arb_sub (depth - 1) >>= fun qt ->
                    oneof[
                      ret_gen (Typing.OType(ot));
                      ret_gen (Typing.QType(vs, qt))
                      ]
    in
    choose_int(0, 3) >>= fun i ->
        arb_sub i
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
let prop_typeVars_duplication : 'a -> bool =
  fun xs ->
      let vs = Typing.typeVars xs in
      List.length vs == List.length (ExtList.List.unique vs)
let () = Check_oType_to_bool.quickCheck prop_typeVars_duplication

let prop_freeTypeVars_duplication : 'a -> bool =
  fun xs ->
      let vs = Typing.freeTypeVars xs in
      List.length vs == List.length (ExtList.List.unique vs)
let () = Check_typeScheme_to_bool.quickCheck prop_freeTypeVars_duplication
