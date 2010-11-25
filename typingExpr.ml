type exprVar = Id.t
type exprConst = Id.t

type expr =
  | E_Constant of Syntax.lit
  | E_Variable of exprVar
  | E_Fun of exprVar * expr
  | E_Apply of expr * expr
  | E_Tuple of expr list
  | E_Vector of expr list
  | E_If of expr * expr * expr
  | E_Let of exprVar * expr * expr
  | E_Fix of exprVar * expr
  | E_Type of expr * TypingType.oType
  | E_Declare of exprVar * TypingType.oType * expr

type t = expr

type exprEnv =
  | ExprEnv of (Id.t * TypingType.oType) list

type substitution =
  | Substitution of exprVar * expr

let extExprEnv = ref []

let getExtExprEnv = !extExprEnv
let setExtExprEnv eenv =
  extExprEnv := eenv
let addExtExprEnv tp =
  extExprEnv := tp :: !extExprEnv
 
let genExprVarNum = ref 0

let genExprVar () =
  genExprVarNum := !genExprVarNum + 1;
  E_Variable (Format.sprintf "#<expr-variable:%d>" !genExprVarNum)

let rec genExprVars n =
  if n > 0
  then genExprVar () :: genExprVars (n - 1)
  else []

exception ExtFun_not_found of string
let getConstantType = function
  | E_Constant c ->
    (match c with
      | Syntax.Unit -> TypingType.O_Constant Type.Unit
      | Syntax.Bool _ -> TypingType.O_Constant Type.Bool
      | Syntax.Int _ -> TypingType.O_Constant Type.Int
      | Syntax.Float _ -> TypingType.O_Constant Type.Float
      | Syntax.Char _ -> TypingType.O_Constant Type.Char
      | Syntax.ExtFun f -> 
        (try
          List.assoc f !extExprEnv
        with
          | Not_found -> raise (ExtFun_not_found f)))
  | _ -> invalid_arg "expected type E_Constant"

let getVariableType env expr =
  match env, expr with
    | TypingType.TypeEnv envList, E_Variable v -> List.assoc v envList
    | _ -> invalid_arg "expected type E_Variable"

let getExprVarName = function
  | E_Variable v -> v
  | _ -> invalid_arg "expected type E_Variable"

let rec getExprType = function
  | E_Type (_, t) -> t
  | E_Let (_, _, e) -> getExprType e
  | E_If (_, _, e) -> getExprType e
  | E_Declare (_, _, e) -> getExprType e
  | E_Fun (_, e) -> getExprType e
  | _ -> invalid_arg "expected type-infered expr"

let rec substituteExpr ss expr =
  let subst = substituteExpr ss in
  match expr with
    | E_Variable v ->
      if List.mem_assoc v ss 
      then List.assoc v ss
      else E_Variable v
    | E_Constant c -> E_Constant c
    | E_Fun(v, e) ->
      let ss' = List.remove_assoc v ss in
      let subst' = substituteExpr ss' in
      E_Fun(v, subst' e)
    | E_Apply(e1, e2) -> E_Apply(subst e1, subst e2)
    | E_Tuple(es) -> E_Tuple(List.map subst es)
    | E_Vector(es) -> E_Vector(List.map subst es)
    | E_If(e1, e2, e3) -> E_If(subst e1, subst e2, subst e3)
    | E_Let(v, e1, e2) ->
      let ss' = List.remove_assoc v ss in
      let subst' = substituteExpr ss' in
      E_Let(v, subst e1, subst' e2)
    | E_Fix(f, e) ->
      let ss' = List.remove_assoc f ss in
      let subst' = substituteExpr ss' in
      E_Fix(f, subst' e)
    | E_Type(e, t) -> E_Type(subst e, t)
    | E_Declare(v, t, e) ->
      let v' = genExprVar () in
      let v'c = getExprVarName v' in
      if List.mem_assoc v ss 
      then E_Let(v'c, List.assoc v ss, E_Declare(v'c, t, subst e))
      else E_Declare(v, t, subst e)

let rec substituteExprType ss expr =
  let subst = substituteExprType ss in
  match expr with
    | E_Variable v -> E_Variable v
    | E_Constant c -> E_Constant c
    | E_Fun(v, e) -> E_Fun(v, subst e)
    | E_Apply(e1, e2) -> E_Apply(subst e1, subst e2)
    | E_Tuple(es) -> E_Tuple(List.map subst es)
    | E_Vector(es) -> E_Vector(List.map subst es)
    | E_If(e1, e2, e3) -> E_If(subst e1, subst e2, subst e3)
    | E_Let(v, e1, e2) -> E_Let(v, subst e1, subst e2)
    | E_Fix(f, e) -> E_Fix(f, subst e)
    | E_Type(e, t) -> E_Type(subst e, TypingType.substitute ss t)
    | E_Declare(v, t, e) -> E_Declare(v, TypingType.substitute ss t, subst e)

(*
type pat =
  | P_Ident of Id.t
  | P_Literal of lit
  | P_Tuple of pat list
  | P_List of pat list
  | P_Array of pat list
  | P_Variant of Id.t * pat list
  | Any
*)

let rec expr_from_syntax = function
  | Syntax.Literal l -> E_Constant l
  | Syntax.Add (e1, e2) -> E_Apply(E_Apply(E_Variable "add", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Sub (e1, e2) -> E_Apply(E_Apply(E_Variable "sub", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Mul (e1, e2) -> E_Apply(E_Apply(E_Variable "mul", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Div (e1, e2) -> E_Apply(E_Apply(E_Variable "div", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Fadd (e1, e2) -> E_Apply(E_Apply(E_Variable "fadd", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Fsub (e1, e2) -> E_Apply(E_Apply(E_Variable "fsub", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Fmul (e1, e2) -> E_Apply(E_Apply(E_Variable "fmul", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Fdiv (e1, e2) -> E_Apply(E_Apply(E_Variable "fdiv", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Seq (e1, e2) -> E_Apply(E_Apply(E_Variable "seq", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.And (e1, e2) -> E_Apply(E_Apply(E_Variable "and", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Or (e1, e2) -> E_Apply(E_Apply(E_Variable "or", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Eq (e1, e2) -> E_Apply(E_Apply(E_Variable "eq", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.NotEq (e1, e2) -> E_Apply(E_Apply(E_Variable "notEq", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.LsEq (e1, e2) -> E_Apply(E_Apply(E_Variable "lsEq", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Ls (e1, e2) -> E_Apply(E_Apply(E_Variable "ls", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.Gt (e1, e2) -> E_Apply(E_Apply(E_Variable "gt", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.GtEq (e1, e2) -> E_Apply(E_Apply(E_Variable "gtEq", expr_from_syntax e1), expr_from_syntax e2)
  | Syntax.LetSimp ((v, t), e1, e2) -> E_Let(v, expr_from_syntax e1, expr_from_syntax e2)
  | Syntax.If (e1, e2, e3) -> E_If(expr_from_syntax e1, expr_from_syntax e2, expr_from_syntax e3)
  | Syntax.Fun (vts, e) -> 
    let f acc v = E_Fun(v, acc) in
    let vs = fst (List.split vts) in
    List.fold_left f (expr_from_syntax e) vs
  | Syntax.Var v -> E_Variable v
  | Syntax.Apply (f, es) -> 
    let g acc e = E_Apply(acc, expr_from_syntax e) in
    List.fold_left g (expr_from_syntax f) es
  | Syntax.Tuple es -> 
    E_Tuple(List.map expr_from_syntax es)
  | Syntax.Cons (e1, e2) -> E_Apply(E_Variable "Cons", E_Tuple[expr_from_syntax e1; expr_from_syntax e2])
  | Syntax.List es -> 
    let f e acc= E_Apply(E_Variable "Cons", E_Tuple[expr_from_syntax e;acc]) in
    List.fold_right f es (E_Variable "Nil")
  | Syntax.Array es -> 
    E_Vector(List.map expr_from_syntax es)
  | _ -> invalid_arg "unexpected Syntax"

let rec to_sexpr = function
  | E_Constant e -> Sexpr.Sexpr [Sexpr.Sident "e:constant"; Syntax.lit_to_sexpr e]
  | E_Variable v -> Sexpr.Sexpr [Sexpr.Sident "e:var"; Sexpr.Sident v]
  | E_Tuple es -> Sexpr.Sexpr (Sexpr.Sident "e:tuple" :: (List.map to_sexpr es))
  | E_Vector es -> Sexpr.Sexpr (Sexpr.Sident "e:vector" :: (List.map to_sexpr es))
  | E_If (e1, e2, e3) -> Sexpr.Sexpr (Sexpr.Sident "e:if" :: (List.map to_sexpr [e1; e2; e3]))
  | E_Let (v, e1, e2) -> Sexpr.Sexpr (Sexpr.Sident "e:let" :: to_sexpr(E_Variable v) :: (List.map to_sexpr [e1; e2]))
  | E_Fix (v, e) -> Sexpr.Sexpr (Sexpr.Sident "e:fix" :: to_sexpr(E_Variable v) :: (List.map to_sexpr [e]))
  | E_Fun(v, e) -> 
    let rec fun_flatten vs = function
      | E_Fun(v, e) ->  fun_flatten (v :: vs) e
      | e -> (List.rev vs), e
    in
    let vs, e' = fun_flatten [] e in
    let vs' = List.map (fun x -> E_Variable x) (v :: vs) in
    Sexpr.Sexpr [Sexpr.Sident "e:fun"; Sexpr.Sexpr(List.map to_sexpr vs'); to_sexpr e']
  | E_Apply(e1, e2) -> 
    let rec apply_flatten = function
      | E_Apply(e1, e2) ->  e1 :: apply_flatten e2
      | e -> [e]
    in
    Sexpr.Sexpr (Sexpr.Sident "e:apply" ::  to_sexpr e1 :: List.map to_sexpr (apply_flatten e2))
  | E_Type(e, t) ->
    Sexpr.Sexpr[Sexpr.Sident "e:type"; to_sexpr e; TypingType.oType_to_sexpr t]
  | E_Declare(v, t, e) ->
    Sexpr.Sexpr[Sexpr.Sident "e:declare"; to_sexpr(E_Variable v); TypingType.oType_to_sexpr t; to_sexpr e]

let rec of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident "e:constant"; e] -> E_Constant (Syntax.lit_of_sexpr e)
  | Sexpr.Sexpr [Sexpr.Sident "e:var"; Sexpr.Sident v] -> E_Variable v
  | Sexpr.Sexpr (Sexpr.Sident "e:tuple" :: es) -> E_Tuple (List.map of_sexpr es)
  | Sexpr.Sexpr (Sexpr.Sident "e:vector" :: es) -> E_Vector (List.map of_sexpr es)
  | Sexpr.Sexpr [Sexpr.Sident "e:if"; e1; e2; e3] -> E_If (of_sexpr e1, of_sexpr e2, of_sexpr e3)
  | Sexpr.Sexpr [Sexpr.Sident "e:let"; v; e1; e2] -> E_Let (getExprVarName (of_sexpr v), of_sexpr e1, of_sexpr e2)
  | Sexpr.Sexpr [Sexpr.Sident "e:fix"; v; e] -> E_Fix (getExprVarName (of_sexpr v), of_sexpr e)
  | Sexpr.Sexpr [Sexpr.Sident "e:fun"; Sexpr.Sexpr vs; e] -> 
    let fun_nest e' vs =
      let rec fun_nest_sub = function
        | [v] ->  E_Fun(getExprVarName(of_sexpr v), of_sexpr e')
        | v :: vs ->  E_Fun(getExprVarName(of_sexpr v), fun_nest_sub vs)
        | _ -> invalid_arg "unexpected token."
      in
      fun_nest_sub vs
    in
    fun_nest e vs
  | Sexpr.Sexpr (Sexpr.Sident "e:apply" :: e1 :: e2 :: es) -> 
    let rec apply_nest = function
      | e1 :: e2 :: []->  E_Apply(of_sexpr e1, of_sexpr e2)
      | e :: es -> E_Apply(of_sexpr e, apply_nest es)
      | _ -> invalid_arg "unexpected token."
    in
    apply_nest (e1 :: e2 :: es)
  | Sexpr.Sexpr [Sexpr.Sident "e:type"; e; t] -> E_Type(of_sexpr e, TypingType.oType_of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "e:declare"; v; t; e] -> E_Declare(getExprVarName(of_sexpr v), TypingType.oType_of_sexpr t, of_sexpr e)
  | _ -> invalid_arg "unexpected token."
 
