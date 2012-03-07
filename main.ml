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
  Debug.dbgprint "Sexpr reading...";
  let syntaxs = TranslationUnit.read stm in
  Debug.dbgprint "Sexpr modulizing...";
  TranslationUnit.modulize (Module.ext_expr_env pervasives) syntaxs

let knormalize_module ch m =
  Debug.dbgprint "coerce typevar to unit.";
  let m = Module.coerce_typevars (TypingType.O_Constant Type.Unit) m in
  Debug.dbgprintsexpr ~level:5 (Module.to_sexpr m);
  Debug.dbgprint "compose modules.";
  Debug.dbgprintsexpr ~level:5 (Module.to_sexpr m);
  let m = Module.compose pervasives m in
  Debug.dbgprint "unfold pattern.";
  Debug.dbgprintsexpr ~level:5 (Module.to_sexpr m);
  let m = Pattern.unfold_module m in
  Debug.dbgprint "instantiate general functions.";
  Debug.dbgprintsexpr ~level:5 (Module.to_sexpr m);
  let m = Instantiate.instantiate m in
  Debug.dbgprint "removing polymorphic function template.";
  let m = Module.remove_polymorphic_functions m in
  Debug.dbgprintsexpr ~level:5 (Module.to_sexpr m);
  Debug.dbgprint "convert expr to K-normal.";
  (* let s,t = KNormal.from_typing_result r in*)
  let s = KNormal.from_module m in
  Debug.dbgprintsexpr ~level:5 (KNormal.topDecls_to_sexpr s);
  s 
(*i  let k = fst (KNormal.from_typing_result r) in
  Debug.dbgprint "Alpha transform.";
  Alpha.f k i*)

let optimize_knormal k = k

let compile_knormal ch k =
  let c = try
  Closure.from_knormal_topdecls k
  with Not_found -> failwith "end closure" in
  Debug.dbgprintsexpr (Closure.topDecls_to_sexpr c);
  Debug.dbgprint "compile to asm.";
  let va = VirtualAsm.f c in
  va

let emit_asm ch (funcs, fp_table) =
  let f = List.map Asmx86.generate_function (Basicblock.f funcs) in
    List.iter (Asmx86.output_function ch) f
  

let compile ch stm =
  Debug.dbgprint "reading...";
  let m = read_module stm in
  Debug.dbgprint "K-normalizing...";
  let k = knormalize_module ch m in
  Debug.dbgprint "optimizing...";
  let k' = optimize_knormal k in
  Debug.dbgprintsexpr (KNormal.topDecls_to_sexpr k');
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
