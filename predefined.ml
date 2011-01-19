open MyUtil

module T = Typing
module TE = TypingExpr
module TT = TypingType

type internal_def = Id.t * TypingExpr.t

let intop_type tfs tt =
  match tfs with
    | [] -> tt
    | [tf] -> TT.O_Fun (tf, tt)
    | tfs -> TT.O_Fun (TT.O_Tuple tfs, tt)

let intop name tfs tt =
  let rec f = function
    | [], [] -> TE.E_External(name, intop_type tfs tt)
    | [], [v] -> TE.E_Apply (TE.E_External(name, intop_type tfs tt), v)
    | [], vs -> TE.E_Apply (TE.E_External(name, intop_type tfs tt), TE.E_Tuple vs)
    | t :: ts, vs ->
      let b = TE.gen_exprvar () in
      let bn = TE.get_exprvar_name b in
      TE.E_Fun (bn, f (ts, (b :: vs)))
  in
  f (tfs, [])

let intop_t name tfs tt =
  intop name (List.map TT.oType_of_type tfs) (TT.oType_of_type tt)

let pervasives =
  let tyu = Type.Unit in
  let tyi = Type.Int in
  let tygie = [Type.Int] in
  let tygiie = [Type.Int; Type.Int] in
  let tyf = Type.Float in
  let tygfe = [Type.Float] in
  let tygffe = [Type.Float; Type.Float] in
  let tyb = Type.Bool in
  let tyc = Type.Char in
  let tygce = [tyc] in
  let tyva = Type.Var "a" in
  let tygvavae = [tyva; tyva] in
  let tylva = Type.List tyva in
  let tyglvae = [Type.List tyva] in
  let tygvalvae = [tyva; Type.List tyva] in
  let tyava = Type.Array tyva in
  let tyac = Type.Array tyc in
  let tygace = [tyac] in
  let tygavaie = [Type.Array tyva; tyi] in
  let tygavaivae = [Type.Array tyva; tyi; tyva] in
  let tygrvae = [Type.Ref tyva] in
  let tygrvavae = [Type.Ref tyva; tyva] in
  List.fold_left Module.add_expr Module.empty [
    "~", intop_t "%neg" tygie tyi;
    "+", intop_t "%add" tygiie tyi;
    "-", intop_t "%sub" tygiie tyi;
    "*", intop_t "%mul" tygiie tyi;
    "/", intop_t "%div" tygiie tyi;
    "~.", intop_t "%fneg" tygfe tyi;
    "+.", intop_t "%fadd" tygffe tyf;
    "-.", intop_t "%fsub" tygffe tyf;
    "*.", intop_t "%fmul" tygffe tyf;
    "/.", intop_t "%fdiv" tygffe tyf;
    "=", intop_t "%eq" tygvavae tyb;
    "<>", intop_t "%not-eq" tygvavae tyb;
    "<", intop_t "%gt" tygvavae tyb;
    "<=", intop_t "%gt-eq" tygvavae tyb;
    ">", intop_t "%ls" tygvavae tyb;
    ">=", intop_t "%ls-eq" tygvavae tyb;
    "cons", intop_t "%cons" tygvalvae tylva;
    "::", intop_t "%cons" tygvalvae tylva;
    "car", intop_t "%car" tyglvae tyva;
    "hd", intop_t "%car" tyglvae tyva;
    "cdr", intop_t "%cdr" tyglvae tylva;
    "tl", intop_t "%cdr" tyglvae tylva;
    "array-alloc", intop_t "%array-alloc" tygie tyava;
    "array-ref", intop_t "%array-ref" tygavaie tyva;
    "array-set", intop_t "%array-set" tygavaivae tyu;
    "ref", intop_t "%ref" tygrvae tyva;
    "set", intop_t "%set" tygrvavae tyu;
    "ignore", TE.E_Fun ("x", TE.E_Constant Syntax.Unit);
    "print-int", intop_t "print_int" tygie tyu;
    "print-float", intop_t "print_float" tygfe tyu;
    "print-char", intop_t "print_char" tygce tyu;
    "print-string", intop_t "print_string" tygace tyu;
    ":", TE.E_Fun("x", TE.E_Fun("y", TE.E_Variable "y"));
]
 