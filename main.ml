open MyUtil

type action =
  | NklAsm
  | NklCml


let _ = Debug.set_dbglevel 10
  
let read_module name env stm =
  Debug.dbgprint (name ^ " module reading...");
  let syntaxs = TranslationUnit.read stm in
  Debug.dbgprint (name ^ " module modulizing...");
  TranslationUnit.modulize env syntaxs

let knormalize_module ch m =
(*
  Debug.dbgprint "coerce typevar to unit.";
  let m = Module.coerce_typevars (TypingType.O_Constant Type.Unit) m in
  Debug.dbgprintsexpr ~level:5 (Module.to_sexpr m);
*)
  Debug.dbgprint "unfold pattern.";
  let m = Pattern.unfold_module m in
  Debug.dbgprintsexpr ~level:5 (Module.to_sexpr m);
  Debug.dbgprint "instantiate general functions.";
  let m = Instantiate.instantiate m in
  Debug.dbgprintsexpr ~level:5 (Module.to_sexpr m);
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

let emit_asm ch (funcs, fp_table) =
  let f = List.map Asmx86.generate_function (Basicblock.f funcs) in
    List.iter (Asmx86.output_function ch) f
  
let compile_knormal ch k =
  let c = try
  Closure.from_knormal_topdecls k
  with Not_found -> failwith "end closure" in
  Debug.dbgprintsexpr (Closure.topDecls_to_sexpr c);
  Debug.dbgprint "compile to asm.";
  let va = VirtualAsm.f c in
  emit_asm ch va



let compile_to_optimized_knormal ch m =
  Debug.dbgprint "K-normalizing...";
  let k = knormalize_module ch m in
  Debug.dbgprint "optimizing...";
  optimize_knormal k

let compile_to_cml ch m =
  let k = compile_to_optimized_knormal ch m in
  Debug.dbgprintsexpr ~level:5 (KNormal.topDecls_to_sexpr k);
  Sexpr.write (Format.formatter_of_out_channel ch) (KNormal.topDecls_to_sexpr k)

let compile_to_asm ch m =
  let k = compile_to_optimized_knormal ch m in
  Debug.dbgprintsexpr ~level:5 (KNormal.topDecls_to_sexpr k);
  compile_knormal ch k

let compile_stm name env ch stm =
  Debug.dbgprint "reading...";
  let m = read_module name env stm in
  Debug.dbgprint "K-normalizing...";
  let k = knormalize_module ch m in
  Debug.dbgprint "optimizing...";
  let k' = optimize_knormal k in
  Debug.dbgprintsexpr ~level:5 (KNormal.topDecls_to_sexpr k');
  compile_knormal ch k'

let string name env str = compile_stm name env stdout (Stream.of_string str)

let read_module_from_file env f =
  let inchan = open_in (f ^ ".nkl") in
  try
    let m = read_module f env (Stream.of_channel inchan) in
    close_in inchan;
    m
  with e -> (close_in inchan; raise e)

let file f =
  let inchan = open_in (f ^ ".nkl") in
  let outchan = open_out (f ^ ".s") in
  try
    compile_stm f (Module.ext_expr_env Predefined.pervasives) outchan (Stream.of_channel inchan);
    close_in inchan;
    close_out outchan
  with e -> (close_in inchan; close_out outchan; raise e)

let () = 
  let action = ref NklAsm in
  let files = ref [] in
  let out_filename_stem = ref "a" in
  Arg.parse
    [
      "--nkl-to-asm", Arg.Unit (fun () -> action := NklAsm), "set output assembler";
      "--nkl-to-cml", Arg.Unit (fun () -> action := NklCml), "set output intermediate code";
      "-o", Arg.Set_string out_filename_stem, "output file name";
    ]
      (fun s -> files := !files @ [s])
      ("GuNCT nibKame Compiler\n" ^
       Printf.sprintf "usage: %s filenames without \".nkl\"..." Sys.argv.(0));
  let out_filename_ext = match !action with
    | NklAsm -> ".s"
    | NklCml -> ".cml" in
  let out_filename = !out_filename_stem ^ out_filename_ext in
  let outchan = open_out out_filename in
  try
    Debug.dbgprint ~level:10 "Predefined module is:";
    Debug.dbgprintsexpr ~level:10 (Module.to_sexpr Predefined.pervasives);
    let m = List.fold_left
      (fun m f ->
	let m' = read_module_from_file (Module.ext_expr_env m) f in
        Debug.dbgprint ~level:10 (f ^ "module is:");
        Debug.dbgprintsexpr ~level:10 (Module.to_sexpr m);
	Module.compose m m')
      Predefined.pervasives
      !files in
    begin
      match !action with
	| NklAsm ->
	  compile_to_asm outchan m
	| NklCml ->
	  compile_to_cml outchan m end
  with e -> (close_out outchan; raise e)
