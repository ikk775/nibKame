(* type t = string *)
type l = L of string (* label *)

include String

type Id.l = string

module Set_sub = Set.Make(String)
module Set = struct
  include Set_sub
  let of_list lis = List.fold_right Set_sub.add lis Set_sub.empty 
  end

module Map = Map.Make(String)

type substitution = Substitution of t * t    

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

let composeSubsts sss =
  List.fold_right compose sss []

