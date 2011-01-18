
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

let rec read_type = function
  | Sident "unit" -> Type.Unit
  | Sident "bool" -> Type.Bool
  | Sident "int" -> Type.Int
  | Sident "float" -> Type.Float
  | Sident "char" -> Type.Char
  | Sident var -> Type.Var var
  | Sexpr l -> read_typelist l
  | _ -> invalid_arg "unreconized type"
and read_typelist = function
  | Sident "tuple" :: tail -> Type.Tuple (List.map read_type tail)
  | Sident "variant" :: Sident name :: tail -> Type.Variant name
  | [Sident "list"; t] -> Type.List (read_type t)
  | [Sident "array"; t] -> Type.Array (read_type t)
  | [Sident "fun"; Sexpr l; t] -> Type.Fun (List.map read_type l, read_type t)
  | _ -> invalid_arg "unreconized type"

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
      | [Sident "and"; l] -> pattern_of_list l
      | Sident "and" :: l :: ls -> Syntax.P_And (pattern_of_list l, pattern_of_list (Sexpr (Sident "and" :: ls)))
      | [Sident "or"; l] -> pattern_of_list l
      | Sident "or" :: l :: ls -> Syntax.P_Or (pattern_of_list l, pattern_of_list (Sexpr (Sident "or" :: ls)))
      | [Sident "not"; l] -> Syntax.P_Not (pattern_of_list l)
      | Sident constructor :: p -> Syntax.P_Variant (constructor, (List.map pattern_of_list p))
      | _ -> invalid_arg "unexpected pattern."

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
      (match l with
	| Sident "type" :: Sident name :: Sexpr types :: constructors ->
	    let variant = Variant.empty_tags name in
	      List.iter (function
			   | Sexpr [Sident i] ->
			       Variant.add_tag variant i
			   | Sexpr [Sident i; typeexprs] ->
			       Variant.add_constructor variant i [Type.of_sexpr typeexprs]
         | _ -> invalid_arg "unexpected token" )
		        constructors;
	      Variant.add_variant !variant;
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
	| Sident "let" :: pat :: a :: [] ->
	    Syntax.TopLet (pattern_of_list pat, change a)
	| Sident "letrec" :: Sident name :: a :: b :: [] ->
	    Syntax.LetRec ((name, Type.gentype()), change a, change b)
	| Sident "letrec" :: Sident name :: a :: [] ->
	    Syntax.TopLetRec ((name, Type.gentype()), change a)
	| Sident "fun" :: Sexpr l :: a :: [] ->
	    Syntax.Fun (List.map 
			  (function
          | Sident a -> a, Type.gentype()
          | _ -> invalid_arg "unexpected token")
			  l,
			change a)
	| Sident "apply" :: a :: args ->
	    Syntax.Apply (change a, List.map change args)
	
	| any -> invalid_arg "unexpected token."
      )
