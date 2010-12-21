(* type t = string *)

type l = L of string (* label *)

module Set_sub = Set.Make(String)
module Set = struct
  include Set_sub
  let of_list lis = List.fold_right Set_sub.add lis Set_sub.empty 
  end

module Map_sub = Map.Make(String)
module Map = struct
  include Map_sub
  let of_assoc map = 
    let l = ref [] in
    iter (fun k c -> l := (k, c) :: !l) map
  end

type substitution = Substitution of string * string    

let rec substitute ss id =
  match ss, id with 
    | [], x -> x
    | Substitution(fx, tx) :: ss, x  -> 
      if fx = x then tx
      else substitute ss x

let rec compose (xs:substitution list) (ys:substitution list) =
  let subst_tx x ys = (List.map (fun y -> match y with Substitution(fx, tx) -> Substitution (fx ,substitute [x] tx)) ys) in
  match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, ys ->
      if (List.exists (fun y -> 
        match x, y with
          | Substitution(fx, _), Substitution (fy, _) -> fx = fy) ys)
      then compose xs (subst_tx x ys)
      else compose xs (x :: subst_tx x ys)

let compose_substs sss =
  List.fold_right compose sss []

let rec substitution_to_sexpr = function
  | Substitution(fv, tv) -> Sexpr.Sexpr [Sexpr.Sident fv; Sexpr.Sident tv]

let rec substitution_of_sexpr = function
  | Sexpr.Sexpr [Sexpr.Sident fv; Sexpr.Sident tv] -> Substitution (fv, tv)
  | _ -> invalid_arg "unexpected token."

let rec substitutions_to_sexpr = function
  | ss -> Sexpr.Sexpr (List.map substitution_to_sexpr ss)

let rec substitutions_of_sexpr = function
  | Sexpr.Sexpr (ss) -> 
    List.map substitution_of_sexpr ss
  | _ -> invalid_arg "unexpected token."

