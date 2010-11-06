
let list_of_char str =
  let len = String.length str in
  let rec iter c =
    if c < len then
      (String.get str c) :: iter (c + 1)
    else
      []
  in
    iter 0

let list_of_Char str =
  let len = String.length str in
  let rec iter c =
    if c < len then
      Syntax.Literal (Syntax.Char (String.get str c)) :: iter (c + 1)
    else
      []
  in
    iter 0

let list_of_pChar str =
  let len = String.length str in
  let rec iter c =
    if c < len then
      Syntax.P_Literal (Syntax.Char (String.get str c)) :: iter (c + 1)
    else []
  in
    iter 0

open Sexpr

let rec pattern_of_list = function
  | Sstring s -> Syntax.P_Array (list_of_pChar s)
  | Sident "_" -> Syntax.Any
  | Sident ident -> Syntax.P_Ident ident
  | Sint i -> Syntax.P_Literal (Syntax.Int i)
  | Sfloat f -> Syntax.P_Literal (Syntax.Float f)
  | Schar c -> Syntax.P_Literal (Syntax.Char c)
  | Sexpr l -> match l with
      | Sident "list" :: l -> Syntax.P_List (List.map pattern_of_list l)
      | Sident "tuple" :: l -> Syntax.P_Tuple (List.map pattern_of_list l)
      | Sident "array" :: l -> Syntax.P_Array (List.map pattern_of_list l)
      | Sident constructor :: p -> Syntax.P_Variant (pattern_of_list p)

(*
  val change : Sexpr.t -> Syntax.t
  バリアントは副作用によって動作することを前提としている.
*)
let rec change = function
  | Sstring s -> Syntax.Array (list_of_Char s)
  | Sident i -> 
      (match i with
	 | "true" -> Syntax.Literal (Syntax.Bool true)
	 | "false" -> Syntax.Literal (Syntax.Bool false)
	 | "unit" -> Syntax.Literal Syntax.Unit
	 | any -> Syntax.Var any)
  | Sint i -> Syntax.Literal (Syntax.Int i)
  | Sfloat f -> Syntax.Literal (Syntax.Float f)
  | Schar c -> Syntax.Literal (Syntax.Char c)
  | Sexpr l ->
      match l with
	| Sident "type" :: Sident name :: Sexpr types :: constructors ->
	    Syntax.Variant name (* Fixing ME !! *)

	| Sident "list" :: tail -> Syntax.List (List.map change tail)
	| Sident "tuple" :: tail -> Syntax.Tuple (List.map change tail)
	| Sident "array" :: tail -> Syntax.Array (List.map change tail)

	| Sident "+" :: a :: b :: [] -> Syntax.Add (change a, change b)
	| Sident "-" :: a :: b :: [] -> Syntax.Sub (change a, change b)
	| Sident "*" :: a :: b :: [] -> Syntax.Mul (change a, change b)
	| Sident "/" :: a :: b :: [] -> Syntax.Div (change a, change b)
	| Sident "+." :: a :: b :: [] -> Syntax.Fadd (change a, change b)
	| Sident "-." :: a :: b :: [] -> Syntax.Fsub (change a, change b)
	| Sident "*." :: a :: b :: [] -> Syntax.Fmul (change a, change b)
	| Sident "/." :: a :: b :: [] -> Syntax.Fdiv (change a, change b)

	| Sident "=" :: a :: b :: [] -> Syntax.Eq (change a, change b)
	| Sident "<>" :: a :: b :: [] -> Syntax.NotEq (change a, change b)
	| Sident "<=" :: a :: b :: [] -> Syntax.LsEq (change a, change b)
	| Sident "<" :: a :: b :: [] -> Syntax.Ls (change a, change b)
	| Sident ">" :: a :: b :: [] -> Syntax.Gt (change a, change b)
	| Sident ">=" :: a :: b :: [] -> Syntax.GtEq (change a, change b)

	| Sident "cons" :: a :: b :: [] -> Syntax.Cons (change a, change b)
	| Sident ";" :: a :: b :: [] -> Syntax.Seq (change a, change b)

	| Sident "&&" :: a :: b :: [] -> Syntax.And (change a, change b)
	| Sident "||" :: a :: b :: [] -> Syntax.Or (change a, change b)

	| Sident "if" :: a :: b :: c :: [] ->
	    Syntax.If (change a, change b, change c)
	| Sident "let" :: pat :: a :: b :: [] ->
	    Syntax.Let (pattern_of_list pat, change a, change b)
	| Sident "toplec" :: pat :: a :: [] ->
	    Syntax.TopLet (pattern_of_list pat, change a)
	| Sident "letrec" :: Sident name :: a :: b :: [] ->
	    Syntax.LetRec ((name, Type.gentype()), change a, change b)
	| Sident "topletrec" :: Sident name :: a :: [] ->
	    Syntax.TopLetRec ((name, Type.gentype()), change a)
	| Sident "fun" :: Sexpr l :: a :: [] ->
	    Syntax.Fun (List.map 
			  (fun x -> let Sident a = x in a, Type.gentype())
			  l,
			change a)
	| Sident "apply" :: a :: args ->
	    Syntax.Apply (change a, List.map change args)

	| any -> Syntax.Unit
