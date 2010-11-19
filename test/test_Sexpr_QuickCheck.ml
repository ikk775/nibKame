open QuickCheck

open TestUtil

let sexprSpecialLetters = [' ';'(';')';'\'';'\"';'#';'`';'\\']

(* PShow instances *)
(* Arbitrary instances *)
module Arbitrary_short_symbol_string = struct
  type t = string
  let arbitrary =
    elements (MyUtil.List.setDiff (MyUtil.List.iota 0x20 0x7e) (List.append (MyUtil.List.iota 0x30 0x39) (List.map Char.code [' ';'(';')']))) >>= fun a ->
        let str = String.create 1 in
        str.[0] <- Char.chr a;
        ret_gen str
end
module Arbitrary_symbol_string = struct
  type t = string
  let arbitrary =
    let gen_hd_letter =
      elements (MyUtil.List.setDiff (MyUtil.List.iota 0x20 0x7e) (List.append (MyUtil.List.iota 0x30 0x39) (List.map Char.code sexprSpecialLetters))) >>= lift_gen Char.chr
    in
    let gen_tl_letter =
      elements (MyUtil.List.setDiff (MyUtil.List.iota 0x20 0x7e) (List.map Char.code sexprSpecialLetters)) >>= lift_gen Char.chr 
    in
    sized choose_int0 >>=
      vector gen_tl_letter >>= (fun cs ->
         gen_hd_letter >>= (fun c ->
           ret_gen(c :: cs))) >>=
             lift_gen ExtString.String.implode
end

module Arbitrary_Sexpr(Symbol:ARBITRARY_STRING) = struct
  type t = Sexpr.t
  let arbitrary =
    let rec arb_sub depth =
      if depth <= 0
      then
        Arbitrary_string.arbitrary >>= fun str ->
        Symbol.arbitrary >>= fun sym ->
        Arbitrary_int.arbitrary >>= fun i ->
        Arbitrary_ascii_char.arbitrary >>= fun c ->
        Arbitrary_float.arbitrary >>= fun f ->
          oneof[
            ret_gen (Sexpr.Sstring str);
            ret_gen (Sexpr.Sident sym);
            ret_gen (Sexpr.Sint i);
            ret_gen (Sexpr.Schar c);
            ret_gen (Sexpr.Sfloat f);
                              ]
      else
        let arb_list = sized choose_int0 >>= vector (arb_sub (depth - 1))
        in
        arb_list >>= fun e ->
            oneof[
              arb_sub 0;
              ret_gen (Sexpr.Sexpr e);
              ]
    in
    let arb_Sexpr =
      sized (fun i ->
          arb_sub (truncate (log (float_of_int i))))
    in
    let arb_list = sized choose_int0 >>= vector arb_Sexpr
    in
    arb_list >>= fun e ->
        ret_gen (Sexpr.Sexpr e)
end
module Arbitrary_Sexpr_short_symbol = Arbitrary_Sexpr(Arbitrary_short_symbol_string)
module Arbitrary_Sexpr_long_symbol = Arbitrary_Sexpr(Arbitrary_symbol_string)

let () = Random.init 10

(* Testables instances *)
module Testable_Sexpr_to_bool =
  Testable_fun
  (Arbitrary_Sexpr_long_symbol)
  (PShow_Sexpr)
  (Testable_bool) ;;
module Check_fun_Sexpr_to_bool = Check(Testable_Sexpr_to_bool)
module Testable_Sexpr_ss_to_bool =
  Testable_fun
  (Arbitrary_Sexpr_short_symbol)
  (PShow_Sexpr)
  (Testable_bool) ;;
module Check_fun_Sexpr_ss_to_bool = Check(Testable_Sexpr_ss_to_bool)

module Arbitrary_Sexpr_list = Arbitrary_list(Arbitrary_Sexpr_long_symbol)
module PShow_Sexpr_list = PShow_list(PShow_Sexpr) ;;
module Testable_fun_Sexpr_list_to_bool =
  Testable_fun
  (Arbitrary_Sexpr_list)
  (PShow_Sexpr_list)
  (Testable_bool) ;;
module Check_fun_Sexpr_list_to_bool = Check(Testable_fun_Sexpr_list_to_bool)

module Arbitrary_Sexpr_ss_list = Arbitrary_list(Arbitrary_Sexpr_short_symbol)
module Testable_fun_Sexpr_ss_list_to_bool =
  Testable_fun
  (Arbitrary_Sexpr_ss_list)
  (PShow_Sexpr_list)
  (Testable_bool) ;;
module Check_fun_Sexpr_ss_list_to_bool = Check(Testable_fun_Sexpr_ss_list_to_bool)

let prop_read_write : 'a -> bool =
  fun x ->
      try
        let y =
          Sexpr.read
            (Stream.of_string
                (MyUtil.Format.call_with_output_string
                    (fun fmtr ->
                          (Sexpr.write fmtr x))))
        in
        gen_prop_equality Std.dump Sexpr.equal x y
      with
      | Stream.Error s ->
          (Format.printf "Exception: Stream.Error %S@\n@?" s;
            false)
      | End_of_file ->
          (Format.printf "Exception: End_of_file @\n@?";
            false)
      | _ -> false

let () = Check_fun_Sexpr_ss_to_bool.quickCheck prop_read_write
let () = Check_fun_Sexpr_to_bool.quickCheck prop_read_write

