open MyUtil

let pervasives_module_name = "pervasives.nkl"
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

let knormalize_module ch m =
  Debug.dbgprint "compose modules.";
  let m = Module.compose pervasives m in
  Debug.dbgprint "unfold pattern.";
  let m = Pattern.unfold_module m in
  Debug.dbgprint "instantiate general functions.";
  let m = Instantiate.instantiate m in
  Debug.dbgprint "coerce typevar to unit.";
  let m = Module.coerce_typevars (TypingType.O_Constant Type.Unit) m in
  Sexpr.write (Format.formatter_of_out_channel ch) (Module.to_sexpr m);
  Debug.dbgprint "convert module to single expr.";
  let r = Module.gather_expr m in
  Debug.dbgprint "convert expr to K-normal.";
  fst (KNormal.from_typing_result r)
(*  let k = fst (KNormal.from_typing_result r) in
  Debug.dbgprint "Alpha transform.";
  Alpha.f k *)

let optimize_knormal k = k

let compile_knormal ch k =
  let c = try
  Closure.from_knormal k
  with Not_found -> failwith "end closure" in
  Sexpr.write (Format.formatter_of_out_channel ch) (Closure.topDecls_to_sexpr c);
  Debug.dbgprint "compile to asm.";
  let va = VirtualAsm.f c in
  va

let emit_asm ch (funcs, fp_table) =
  let f = List.map Asmx86.generate_function (Basicblock.f funcs) in
    List.iter (Asmx86.output_function ch) f
  

let compile ch stm =
  let m = read_module stm in
  let k = knormalize_module ch m in
  let k' = optimize_knormal k in
  Sexpr.write (Format.formatter_of_out_channel ch) (KNormal.to_sexpr k');
  let va = compile_knormal ch k' in
  emit_asm ch va

let string str = compile stdout (Stream.of_string str)

let file f =
  let inchan = open_in (f ^ ".nkl") in
  let outchan = open_out (f ^ ".s") in
  try
    compile outchan (Stream.of_channel inchan);
    close_in inchan;
    close_out outchan
  with e -> (close_in inchan; close_out outchan; raise e)

let () = 
  let files = ref [] in
  Arg.parse
    [ ]
      (fun s -> files := !files @ [s])
      ("GuNCT nibKame Compiler\n" ^
       Printf.sprintf "usage: %s filenames without \".nkl\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
