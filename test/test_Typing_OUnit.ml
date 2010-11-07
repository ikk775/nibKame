open OUnit

open Typing

let _ =
  run_test_tt_main
    ("typing.ml" >:::
      [
        "typeVars" >:::
        [
          "typeVars(O_Contant)" >::
            (fun () -> assert_equal [] (typeVars O_Constant)) 
            ;
          "typeVars(O_Variable \"a\")" >::
            (fun () -> assert_equal ["a"] (typeVars (O_Variable "a")))
            ;
          "typeVars(O_Fun(O_Constant, O_Constant))" >::
            (fun () -> assert_equal [] (typeVars (O_Fun(O_Constant, O_Constant))))
            ;
          "typeVars(O_Fun(O_Variable \"a\", O_Constant))" >::
            (fun () -> assert_equal ["a"] (typeVars (O_Fun(O_Variable "a", O_Constant))))
            ;
          "typeVars(O_Fun(O_Constant, O_Variable \"a\"))" >::
            (fun () -> assert_equal ["a"] (typeVars (O_Fun(O_Constant, O_Variable "a"))))
            ;
          "typeVars(O_Fun(O_Variable \"a\", O_Variable \"a\"))" >::
            (fun () -> assert_equal ["a"] (typeVars (O_Fun(O_Variable "a", O_Variable "a"))))
            ;
          "typeVars(O_Fun(O_Variable \"a\", O_Variable \"b\"))" >::
            (fun () -> assert_equal ["a";"b"] (typeVars (O_Fun(O_Variable "a", O_Variable "b"))))
            ;
          "typeVars (O_Fun(O_Fun(O_Variable \"a\", O_Variable \"a\"), O_Fun(O_Variable \"a\", O_Variable \"a\")))" >::
            (fun () -> assert_equal ["a"] (typeVars (O_Fun(O_Fun(O_Variable "a", O_Variable "a"), O_Fun(O_Variable "a", O_Variable "a")))))
          ]
          ;
          "freeVariables" >:::
          [
            "freeVariables OType(O_Constant)" >::
            (fun () -> assert_equal [] (freeTypeVars(OType(O_Constant)))) 
            ;
          ]
      ])
