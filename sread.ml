
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

exception Unexpected_token of Sexpr.t
exception Unexpected_pattern of Sexpr.t
exception Unrecognized_type of Sexpr.t

let unexpected_token s = raise (Unexpected_token s)
let unexpected_pattern s = raise (Unexpected_pattern s)
let unrecognized_type s = raise (Unrecognized_type s)

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
      | _ -> unexpected_pattern (Sexpr l)

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
	 | "nil" -> Syntax.Literal Syntax.Nil
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
      
        | Sident "and" :: [] ->
	    Syntax.Literal (Syntax.Bool true)
        | Sident "and" :: e :: [] ->
	    change e
	| Sident "and" :: e :: es ->
	    Syntax.If (change e, change (Sexpr (Sident "and" :: es)), Syntax.Literal (Syntax.Bool false))

        | Sident "or" :: [] ->
	    Syntax.Literal (Syntax.Bool true)
        | Sident "or" :: e :: [] ->
	    change e
	| Sident "or" :: e :: es ->
	    Syntax.If (change e, Syntax.Literal (Syntax.Bool true), change (Sexpr (Sident "and" :: es)))

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
	| f :: (arg' :: args' as args) ->
	    Syntax.Apply (change f, List.map change args)
	| any -> unexpected_token (Sexpr any)
      )
