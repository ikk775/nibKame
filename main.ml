open MyUtil

let pervasives_module_name = "pervasives.snkl"
let _ = Debug.set_dbglevel 10

let pervasives =
  Debug.dbgprint "open Pervasives module.";
  let ch = open_in pervasives_module_name in
  Debug.dbgprint "opened channel to Pervasives module.";
  let syntaxs = TranslationUnit.read (Stream.of_channel ch) in
  Debug.dbgprint "read syntaxes of Pervasives module.";
  let m = TranslationUnit.modulize (Module.ext_expr_env Predefined.pervasives) syntaxs in
  Debug.dbgprint "modulized Pervasives module.";
  Module.compose Predefined.pervasives m
  
let read_module stm =
  let syntaxs = TranslationUnit.read stm in
  TranslationUnit.modulize (Module.ext_expr_env pervasives) syntaxs

let knormalize_module m =
  let m = Module.compose pervasives m in
  let m = Pattern.unfold_module m in
  let m = Instantiate.instantiate m in
  let r = Module.gather_expr m in
  fst (KNormal.from_typing_result r)

let optimize_knormal k = k

let compile_knormal k =
  let c = Closure.from_knormal k in
  let va = VirtualAsm.f c in
  va

let emit_asm ch va =
  (undefined ())

let compile ch stm =
  let m = read_module stm in
  let k = knormalize_module m in
  let k' = optimize_knormal k in
  let va = compile_knormal k' in
  Printf.fprintf ch "%s" (Std.dump va)

let string str = compile stdout (Stream.of_string str)

let file f =
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    compile outchan (Stream.of_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = 
  let files = ref [] in
  Arg.parse
    [ ]
      (fun s -> files := !files @ [s])
      ("GuNCT nibKame Compiler\n" ^
       Printf.sprintf "usage: %s filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
