open MyUtil

type t =
  | Unit
  | Bool
  | Int
  | Float
  | Char
  | Fun of t list * t
  | Tuple of t list
  | List of t
  | Array of t
  | Ref of t
  | Variant of Id.t
  | Var of Id.t
 
type listCategory =
  | List_Float
  | List_Other

type usingCategory =
  | U_Unit
  | U_Bool
  | U_Int
  | U_Float
  | U_Char
  | U_Tuple of usingCategory list
  | U_Fun of usingCategory list * usingCategory
  | U_Ref of usingCategory
  | U_List of usingCategory
  | U_Array of usingCategory
  | U_Variant of Id.t

type refCategory =
  | R_Unit
  | R_Int
  | R_Float
  | R_Char
  | R_Tuple
  | R_Ref
  | R_Variant
(*i
type pointer =
  | P_List of mType
  | P_Array of mType
  | P_Tuple of mType list
  | P_Ref of mType

type mType =
  | C_Unit
  | C_Int
  | C_Float
  | C_Char
  | C_List
  | C_FList
  | C_Array
  | C_CArray
  | C_FArray
  | C_Ref
i*)

type accCategory =
  | A_Unit
  | A_Int
  | A_Float
  | A_Char
  | A_Tuple
  | A_Ref
  | A_Variant

type exprCategory =
  | E_Unit
  | E_Int
  | E_Float
  | E_Char
  | E_List
  | E_FList
  | E_Array
  | E_FArray
  | E_Tuple of refCategory list
  | E_Ref
  | E_Variant

let rec to_uc = function
  | Unit -> U_Unit
  | Bool -> U_Bool
  | Int -> U_Int
  | Float -> U_Float
  | Char -> U_Char
  | Fun (fts, tt) -> U_Fun (List.map to_uc fts, to_uc tt)
  | Tuple ts -> U_Tuple (List.map to_uc ts)
  | List t -> U_List (to_uc t)
  | Array t -> U_Array (to_uc t)
  | Ref t -> U_Ref (to_uc t)
  | Variant id -> U_Variant id
  | Var _ -> invalid_arg "type variable is not allowed."

let rec to_rc = function
  | _ -> undefined ()

let rec to_ac = function
  | _ -> undefined ()

let rec to_ec = function
  | _ -> undefined ()

type mType = C_Unit | C_Int | C_Float | C_Char | C_Tuple | C_Ref | C_Variant

let rec to_mt = function
  | Unit -> C_Unit
  | Bool -> C_Variant
  | Int -> C_Int
  | Float -> C_Float
  | Char -> C_Char
  | Fun _ -> C_Tuple
  | Tuple _ -> C_Tuple
  | List _ -> C_Tuple
  | Array _ -> C_Ref
  | Ref _ -> C_Ref
  | Variant _ -> C_Variant
  | Var _ -> invalid_arg "Var is not expected."

let gentypenum = ref 0

let gentype () =
  gentypenum := !gentypenum + 1;
  Var (Format.sprintf "$t:%d" !gentypenum)

let rec mt_equal x y =
  to_mt x = to_mt y

let rec equal x y =
  match x, y with
    | Unit, Unit -> true
    | Bool, Bool -> true
    | Int, Int -> true
    | Float, Float -> true
    | Char, Char -> true
    | Fun(ts1, t1), Fun(ts2, t2) -> List.for_all2 equal ts1 ts2 && equal t1 t2
    | Tuple(ts1), Tuple(ts2) -> List.for_all2 equal ts1 ts2
    | List(t1), List(t2) -> equal t1 t2
    | Array(t1), Array(t2) -> equal t1 t2
    | Ref(t1), Ref(t2) -> equal t1 t2
    | Variant(id1), Variant(id2) -> id1 = id2
    | Var(id1), Var(id2) -> id1 = id2
    | _ -> false

let rec to_string = function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "i"
  | Float -> "f"
  | Char -> "c"
  | Fun (t1s, t2) ->
    "m" ^ Mangle.write_seq to_string t1s ^ to_string t2
  | Tuple ts -> 
    "t" ^ Mangle.write_seq to_string ts
  | List t -> 
    "l" ^ to_string t
  | Array t -> 
    "a" ^ to_string t
  | Ref t -> 
    "r" ^ to_string t
  | Variant x -> 
    "d" ^ Mangle.write_id x
  | Var x -> 
    "v" ^ Mangle.write_id x

let rec typed_id_to_string cls id t =
  "_" ^ cls ^ "__" ^ Mangle.write_id id ^ "_T" ^ to_string t
    
let rec g stm =
  let rec h () =
    match Stream.next stm with
      | 'u' -> Unit
      | 'b' -> Bool
      | 'i' -> Int
      | 'f' -> Float
      | 'c' -> Char
      | 'm' ->
        let args = Mangle.read_seq stm g in
        let rst = h () in
        Fun (args, rst)
      | 't' ->
        let ts = Mangle.read_seq stm g in
        Tuple ts
      | 'a' ->
        let t = h () in
        Array t
      | 'l' ->
        let t = h () in
        List t
      | 'r' ->
        let t = h () in
        Ref t
      | 'd' -> 
        let x = Mangle.read_id stm in
        Variant x
      | 'v' -> 
        let x = Mangle.read_id stm in
        Var x
      | _ -> 
        invalid_arg "of_string"
  in
  h ()

let read_from_stream stm =
  g stm

let of_string str =
  g (Stream.of_string str)

let rec of_sexpr = function
  | Sexpr.Sident "t:unit" -> Unit
  | Sexpr.Sident "t:bool" -> Bool
  | Sexpr.Sident "t:int" -> Int
  | Sexpr.Sident "t:float" -> Float
  | Sexpr.Sident "t:char" -> Char
  | Sexpr.Sexpr [Sexpr.Sident "t:fun"; Sexpr.Sexpr t1s; t2] -> 
    Fun (List.map of_sexpr t1s, of_sexpr t2)
  | Sexpr.Sexpr (Sexpr.Sident "t:tuple" :: ts) -> 
    Tuple (List.map of_sexpr ts)
  | Sexpr.Sexpr [Sexpr.Sident "t:list"; t] -> 
    List (of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "t:array";  t] -> 
    Array (of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "t:ref";  t] -> 
    Ref (of_sexpr t)
  | Sexpr.Sexpr [Sexpr.Sident "t:variant"; Sexpr.Sident t] -> 
    Variant t
  | Sexpr.Sexpr [Sexpr.Sident "t:var"; Sexpr.Sident x] -> 
    Var x
  | _ -> invalid_arg "unexpected Type.t" 


let rec to_sexpr = function
  | Unit -> Sexpr.Sident "t:unit"
  | Bool -> Sexpr.Sident "t:bool"
  | Int -> Sexpr.Sident "t:int"
  | Float -> Sexpr.Sident "t:float"
  | Char -> Sexpr.Sident "t:char"
  | Fun (t1s, t2) ->
    Sexpr.Sexpr [Sexpr.Sident "t:fun"; Sexpr.Sexpr (List.map to_sexpr t1s); to_sexpr t2]
  | Tuple ts -> 
    Sexpr.Sexpr (Sexpr.Sident "t:tuple" :: (List.map to_sexpr ts))
  | List t -> 
    Sexpr.Sexpr [Sexpr.Sident "t:list";  to_sexpr t]
  | Array t -> 
    Sexpr.Sexpr [Sexpr.Sident "t:array";  to_sexpr t]
  | Ref t -> 
    Sexpr.Sexpr [Sexpr.Sident "t:ref";  to_sexpr t]
  | Variant t -> 
    Sexpr.Sexpr [Sexpr.Sident "t:variant";  Sexpr.Sident t]
  | Var x -> 
    Sexpr.Sexpr [Sexpr.Sident "t:var";  Sexpr.Sident x]
   
let rec listCategory_to_sexpr = function
  | List_Float -> Sexpr.Sident "lc:float"
  | List_Other -> Sexpr.Sident "lc:other"

let rec listCategory_of_sexpr = function
  | Sexpr.Sident "lc:float" -> List_Float
  | Sexpr.Sident "lc:other" -> List_Other
