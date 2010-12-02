(* type t = string *)
include String

module Set_sub = Set.Make(String)
module Set = struct
  include Set_sub
  let of_list lis = List.fold_right Set_sub.add lis Set_sub.empty 
  end


