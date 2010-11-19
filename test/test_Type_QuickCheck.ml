open QuickCheck
open TestUtil

(* PShow instances *)
module PShow_type = struct
  type t = Type.t
  let show : t -> pretty_str =
    fun s fmt () ->
      Sexpr.write fmt (Type.to_sexpr s)
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
module Arbitrary_type = struct
  type t = Type.t
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_string.arbitrary >>= fun s ->
        oneof[
          ret_gen (Type.Unit);
          ret_gen (Type.Bool);
          ret_gen (Type.Int);
          ret_gen (Type.Float);
          ret_gen (Type.Char);
          ret_gen (Type.Variant s);
          ]
      else
        Arbitrary_string.arbitrary >>= fun s ->
        arb_sub (depth - 1) >>= fun t ->
        logsized choose_int0 >>= lift_gen (fun i -> i + 1) >>= vector (arb_sub (depth - 1)) >>= fun ts ->
        oneof[
          arb_sub 0;
          ret_gen (Type.Fun (ts, t));
          ret_gen (Type.Tuple (ts));
          ret_gen (Type.List (t));
          ret_gen (Type.Array (t));
          oneof[
            ret_gen (Type.Var (ref None));
            ret_gen (Type.Var (ref (Some t)));
            ];
          ]
    in
    sized (fun i ->
        arb_sub (truncate (log (float_of_int i))))
end
module Arbitrary_string_list = Arbitrary_list(Arbitrary_string)

(* Testables instances *)
module Testable_type_to_bool =
  Testable_fun
  (Arbitrary_type)
  (PShow_type)
  (Testable_bool) ;;
module Check_type_to_bool = Check(Testable_type_to_bool)

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
