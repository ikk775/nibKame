type lit =
  | Unit
  | Nil
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | ExtFun of string

type pat =
  | P_Ident of Id.t
  | P_Literal of lit
  | P_Tuple of pat list
  | P_List of pat list
  | P_Array of pat list
  | P_Variant of Id.t * pat list
  | Any
  | P_And of pat * pat
  | P_Or of pat * pat
  | P_Not of pat

type t =
  | Literal of lit
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Fadd of t * t
  | Fsub of t * t
  | Fmul of t * t
  | Fdiv of t * t
  | Cons of t * t
  | Seq of t * t
  | And of t * t
  | Or of t * t
  | Eq of t * t
  | NotEq of t * t
  | LsEq of t * t
  | Ls of t * t
  | Gt of t * t
  | GtEq of t * t
  | Let of pat * t * t
  | LetSimp of (Id.t * Type.t) * t * t
  | LetRec of (Id.t * Type.t) * t * t
  | TopLet of pat * t
  | TopLetSimp of (Id.t * Type.t) * t
  | TopLetRec of (Id.t * Type.t) * t
  | If of t * t * t
  | Variant of Id.t
  | Fix of (Id.t * Type.t) * t
  | Fun of (Id.t * Type.t) list * t
  | Var of Id.t
  | Apply of t * t list
  | Tuple of t list
  | Array of t list
  | List of t list
  | Match of t * (pat * t option * t) list

let lit_to_sexpr = function
  | Unit -> Sexpr.Sident "unit"
  | Nil -> Sexpr.Sident "unit"
  | Bool true -> Sexpr.Sident "true"
  | Bool false -> Sexpr.Sident "false"
  | Int i -> Sexpr.Sint i
  | Float f -> Sexpr.Sfloat f
  | Char c -> Sexpr.Schar c
  | ExtFun f -> Sexpr.Sident f

let lit_of_sexpr = function
  | Sexpr.Sident "unit" -> Unit
  | Sexpr.Sident "nil" -> Nil
  | Sexpr.Sident "true" -> Bool true
  | Sexpr.Sident "false" -> Bool false
  | Sexpr.Sint i -> Int i
  | Sexpr.Sfloat f -> Float f
  | Sexpr.Schar c -> Char c
  | Sexpr.Sident f -> ExtFun f
  | _ -> invalid_arg "unexpected token."

let rec pat_to_sexpr = function
  | P_Ident a -> Sexpr.Sident a
  | P_Literal lit -> lit_to_sexpr lit
  | P_Tuple ps ->  Sexpr.Sexpr (Sexpr.Sident "tuple" :: List.map pat_to_sexpr ps)
  | P_List ps ->  Sexpr.Sexpr (Sexpr.Sident "list" :: List.map pat_to_sexpr ps)
  | P_Array ps ->  Sexpr.Sexpr (Sexpr.Sident "array" :: List.map pat_to_sexpr ps)
  | P_And (p1, p2) -> Sexpr.Sexpr [Sexpr.Sident "and"; pat_to_sexpr p1; pat_to_sexpr p2]
  | P_Or (p1, p2) -> Sexpr.Sexpr [Sexpr.Sident "or"; pat_to_sexpr p1; pat_to_sexpr p2]
  | P_Not p -> Sexpr.Sexpr [Sexpr.Sident "not"; pat_to_sexpr p]
  | P_Variant (id, ps) -> Sexpr.Sexpr (Sexpr.Sident id :: List.map pat_to_sexpr ps)
  | Any -> Sexpr.Sident "_"

let rec pat_of_sexpr = function
  | Sexpr.Sexpr (Sexpr.Sident "tuple" :: ps) -> P_Tuple(List.map pat_of_sexpr ps)
  | Sexpr.Sexpr (Sexpr.Sident "list" :: ps) -> P_List(List.map pat_of_sexpr ps)
  | Sexpr.Sexpr (Sexpr.Sident "array" :: ps) -> P_Array(List.map pat_of_sexpr ps)
  | Sexpr.Sexpr (Sexpr.Sident "and" :: p1 :: p2 :: []) -> P_And(pat_of_sexpr p1, pat_of_sexpr p2)
  | Sexpr.Sexpr (Sexpr.Sident "or" :: p1 :: p2 :: []) -> P_Or(pat_of_sexpr p1, pat_of_sexpr p2)
  | Sexpr.Sexpr (Sexpr.Sident "not" :: p :: []) -> P_Not(pat_of_sexpr p)
  | Sexpr.Sexpr (Sexpr.Sident id :: ps) -> P_Variant(id, List.map pat_of_sexpr ps)
  | Sexpr.Sident "_" -> Any
  | Sexpr.Sident a -> P_Ident a
  | lit -> P_Literal(lit_of_sexpr lit)

let rec of_sexpr = function
  | Literal lit -> lit_to_sexpr lit
 
let rec to_sexpr = function
  | lit -> lit_of_sexpr lit

let mangle qualifiers prefix name t =
  "_nk" ^ "_N" ^ Mangle.write_seq Mangle.write_id qualifiers ^ Mangle.write_id prefix ^ Mangle.write_id name ^ "_T" ^ Type.to_string t

let demangle stm =
  match Stream.npeek 3 stm with
    | ['_'; 'n'; 'k'] -> 
      begin match Stream.npeek 2 stm with
        | ['_'; 'N'] ->
          let qualifiers = Mangle.read_seq stm Mangle.read_id in
          let prefix = Mangle.read_id stm in
          let name = Mangle.read_id stm in
          begin match Stream.npeek 2 stm with
            | ['_'; 'T'] ->
              let t = Type.read_from_stream stm in
              qualifiers, prefix, name, t
            | _ -> invalid_arg "demangle"
          end
        | _ -> invalid_arg "demangle"
      end
    | _ -> invalid_arg "demangle"
