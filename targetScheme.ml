open Sexpr
open KNormal
module S = Sexpr

let rec knormal_to_scheme = function
  | Unit -> Sident "#f"
  | Nil t -> Sident "()"
  | Int i -> Sint i
  | Char c -> Schar c
  | Float x -> Sfloat x
  | Neg id -> tagged_sexpr "-" [ident id]
  | Add (id1, id2) -> tagged_sexpr "+" (List.map ident [id1; id2])
  | Sub (id1, id2) -> tagged_sexpr "-" (List.map ident [id1; id2])
  | Mul (id1, id2) -> tagged_sexpr "*" (List.map ident [id1; id2])
  | Div (id1, id2) -> tagged_sexpr "/" (List.map ident [id1; id2])
  | FNeg id -> tagged_sexpr "-" [ident id]
  | FAdd (id1, id2) -> tagged_sexpr "+" (List.map ident [id1; id2])
  | FSub (id1, id2) -> tagged_sexpr "-" (List.map ident [id1; id2])
  | FMul (id1, id2) -> tagged_sexpr "*" (List.map ident [id1; id2])
  | FDiv (id1, id2) -> tagged_sexpr "/" (List.map ident [id1; id2])
  | If (comp, id1, id2, t1, t2) -> tagged_sexpr "if" (
    tagged_sexpr (
      match comp with
	| Eq -> "="
	| NotEq -> "not-="
	| Ls -> "<"
	| LsEq -> "<="
	| Gt -> ">"
	| GtEq -> ">="
    ) (List.map ident [id1; id2])
    :: (List.map knormal_to_scheme [t1; t2])
  )
  | Let ((id, _), t1, t2) -> tagged_sexpr "let" (
    Sexpr [
      tagged_sexpr id [knormal_to_scheme t1]
    ] :: [knormal_to_scheme t2])
  | Var id -> Sident id
  | Apply ((id, _), args) -> tagged_sexpr id (List.map ident args)
  | Tuple ids -> tagged_sexpr "list" (List.map ident ids)
  | LetTuple (vts, id, t) -> tagged_sexpr "match-let" (
    Sexpr [
      Sexpr [
	Sexpr (List.map (fun x -> ident (fst x)) vts);
	ident id
      ]
    ] :: [knormal_to_scheme t])
  | Ref id -> ident id
  | Set (id1, id2) -> tagged_sexpr "set!" (List.map ident [id1; id2])
  | ArrayAlloc (_, n) -> tagged_sexpr "make-vector" [Sident n]
  | ArrayRef (arr, n) -> tagged_sexpr "vector-ref" (List.map ident [arr; n])
  | ArraySet (arr, n, x) -> tagged_sexpr "vector-set!" (List.map ident [arr; n; x])
  | Cons (id1, id2) -> tagged_sexpr "cons" (List.map ident [id1; id2])
  | Car (id) -> tagged_sexpr "car" (List.map ident [id])
  | Cdr (id) -> tagged_sexpr "cdr" (List.map ident [id])
  | FCons (id1, id2) -> tagged_sexpr "cons" (List.map ident [id1; id2])
  | FCar (id) -> tagged_sexpr "car" (List.map ident [id])
  | FCdr (id) -> tagged_sexpr "cdr" (List.map ident [id])
  | ExtArray id -> Sident id
  | ExtFunApply ((id, _), args) -> tagged_sexpr id (List.map ident args)

let fundef_to_scheme fd =
  tagged_sexpr "define" (
    tagged_sexpr (fst fd.name) (
      List.map (fun x -> ident (fst x)) fd.args
    ) :: [knormal_to_scheme fd.body])

let topvar_to_scheme vd =
  tagged_sexpr "define" [Sident (fst vd.var_name); knormal_to_scheme vd.expr]

let topDecl_to_scheme = function
  | FunDecl fd -> fundef_to_scheme fd
  | VarDecl vd -> topvar_to_scheme vd

let output kn =
  List.map topDecl_to_scheme kn
