open QuickCheck
open TestUtil

(* PShow instances *)
module PShow_literal = struct
  type t = Syntax.lit
  let show : t -> pretty_str =
    fun s fmt () ->
      Sexpr.write fmt (Syntax.lit_to_sexpr s)
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

module Arbitrary_lit = struct
  type t = Syntax.lit
  let arbitrary =
    Arbitrary_string.arbitrary >>= fun s ->
    Arbitrary_int.arbitrary >>= fun i -> 
    Arbitrary_bool.arbitrary >>= fun b -> 
    Arbitrary_float.arbitrary >>= fun f -> 
    Arbitrary_char.arbitrary >>= fun c -> 
      oneof[
        ret_gen (Syntax.Unit);
        ret_gen (Syntax.Bool b);
        ret_gen (Syntax.Char c);
        ret_gen (Syntax.Int i);
        ret_gen (Syntax.Float f);
        ret_gen (Syntax.ExtFun s);
      ]
end
module Arbitrary_pat = struct
  type t = Syntax.pat
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_string.arbitrary >>= fun s ->
        Arbitrary_lit.arbitrary >>= fun lit -> 
        oneof[
          ret_gen (Syntax.P_Literal lit);
          ret_gen (Syntax.P_Ident s);
          ret_gen (Syntax.Any);
          ]
      else
        Arbitrary_string.arbitrary >>= fun s ->
        sized choose_int0 >>= vector (arb_sub (depth - 1)) >>= fun ps ->
        oneof[
          arb_sub 0;
          ret_gen (Syntax.P_Tuple ps);
          ret_gen (Syntax.P_List ps);
          ret_gen (Syntax.P_Array ps);
          ret_gen (Syntax.P_Variant (s, ps));
          ]
    in
    sized (fun i ->
        arb_sub (truncate (log (float_of_int i))))
end
module Arbitrary_string_list = Arbitrary_list(Arbitrary_string)

(* Testables instances *)

(* Tests *)

