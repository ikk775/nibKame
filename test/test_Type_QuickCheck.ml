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
          ret_gen (Type.Var s)
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

