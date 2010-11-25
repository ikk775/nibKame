open QuickCheck
open TestUtil
open Test_Sexpr_QuickCheck

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
      | _ as s -> (Format.printf "Exception: %s %s @\n@?" (Std.dump s) (Std.dump x);
        (Sexpr.write Format.std_formatter x);
            false)

let () = Check_fun_Sexpr_ss_to_bool.quickCheck prop_read_write
let () = Check_fun_Sexpr_to_bool.quickCheck prop_read_write

