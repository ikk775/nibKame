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
  | Variant of Id.t
  | Var of Id.t
 
let gentypenum = ref 0

let gentype () =
  gentypenum := !gentypenum + 1;
  Var (Format.sprintf "$t:%d" !gentypenum)

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
    | Variant(id1), Variant(id2) -> id1 = id2
    | Var(id1), Var(id2) -> id1 = id2
    | _ -> false

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
  | Variant t -> 
    Sexpr.Sexpr [Sexpr.Sident "t:variant";  Sexpr.Sident t]
  | Var x -> 
    Sexpr.Sexpr [Sexpr.Sident "t:var";  Sexpr.Sident x]

