open QuickCheck
open TestUtil
module TestSyntax = Test_Syntax_QuickCheck
module TestTypingType = Test_TypingType_QuickCheck

(* PShow instances *)
module PShow_result = struct
  type t = Typing.result
  let show : t -> pretty_str =
    fun s fmt () ->
      Sexpr.write fmt (Typing.to_sexpr s)
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

module Arbitrary_result = struct
  type t = Typing.result
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_string.arbitrary >>= fun s ->
        TestSyntax.Arbitrary_lit.arbitrary >>= fun lit -> 
        TestTypingType.Arbitrary_oType.arbitrary >>= fun ot -> 
           oneof[
             ret_gen (Typing.R_Constant (lit, ot));
             ret_gen (Typing.R_Variable (s, ot));
             ]
      else
        Arbitrary_string.arbitrary >>= fun s ->
        Arbitrary_string.arbitrary >>= fun s' ->
        arb_sub (depth - 1) >>= fun e ->
        arb_sub (depth - 1) >>= fun e1 ->
        arb_sub (depth - 1) >>= fun e2 ->
        TestTypingType.Arbitrary_oType.arbitrary >>= fun ot -> 
        TestTypingType.Arbitrary_oType.arbitrary >>= fun ot' -> 
        logsized choose_int0 >>= lift_gen (fun i -> i + 1) >>= vector (arb_sub (depth - 1)) >>= fun es ->
          oneof[
            arb_sub 0;
            ret_gen (Typing.R_Fun((s, ot), e));
            ret_gen (Typing.R_Apply(e1, e2));
            ret_gen (Typing.R_Tuple(es, ot));
            ret_gen (Typing.R_Vector(es, ot));
            ret_gen (Typing.R_If(e, e1, e2));
            ret_gen (Typing.R_Let((s, ot), e1, e2));
            ret_gen (Typing.R_Fix((s, ot), Typing.R_Fun((s', ot'), e), ot));
            ]
    in
    sized (fun i ->
        arb_sub (truncate (log (float_of_int i))))
end

module Arbitrary_string_list = Arbitrary_list(Arbitrary_string)

(* Testables instances *)
module Testable_result_to_bool =
  Testable_fun
  (Arbitrary_result)
  (PShow_result)
  (Testable_bool) ;;
module Check_fun_result_to_bool = Check(Testable_result_to_bool)
