open QuickCheck
open TestUtil
module TestSyntax = Test_Syntax_QuickCheck
module TestTypingType = Test_TypingType_QuickCheck

(* PShow instances *)
module PShow_expr = struct
  type t = TypingExpr.expr
  let show : t -> pretty_str =
    fun s fmt () ->
      Sexpr.write fmt (TypingExpr.to_sexpr s)
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

module Arbitrary_expr = struct
  type t = TypingExpr.expr
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_string.arbitrary >>= fun s ->
        TestSyntax.Arbitrary_lit.arbitrary >>= fun lit -> 
           oneof[
             ret_gen (TypingExpr.E_Constant lit);
             ret_gen (TypingExpr.E_Variable s);
             ]
      else
        Arbitrary_string.arbitrary >>= fun s ->
        Arbitrary_string.arbitrary >>= fun s' ->
        arb_sub (depth - 1) >>= fun e ->
        arb_sub (depth - 1) >>= fun e1 ->
        arb_sub (depth - 1) >>= fun e2 ->
        TestTypingType.Arbitrary_oType.arbitrary >>= fun ot -> 
        logsized choose_int0 >>= lift_gen (fun i -> i + 1) >>= vector (arb_sub (depth - 1)) >>= fun es ->
          oneof[
            arb_sub 0;
            ret_gen (TypingExpr.E_Fun(s, e));
            ret_gen (TypingExpr.E_Apply(e1, e2));
            ret_gen (TypingExpr.E_Vector(es));
            ret_gen (TypingExpr.E_If(e, e1, e2));
            ret_gen (TypingExpr.E_Let(s, e1, e2));
            ret_gen (TypingExpr.E_Fix(s, TypingExpr.E_Fun(s', e)));
            ret_gen (TypingExpr.E_Type (e, ot));
            ret_gen (TypingExpr.E_Declare (s, ot, e));
            ]
    in
    sized (fun i ->
        arb_sub (truncate (log (float_of_int i))))
end

module Arbitrary_string_list = Arbitrary_list(Arbitrary_string)

(* Testables instances *)
module Testable_expr_to_bool =
  Testable_fun
  (Arbitrary_expr)
  (PShow_expr)
  (Testable_bool) ;;
module Check_fun_expr_to_bool = Check(Testable_expr_to_bool)
