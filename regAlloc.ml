module VA = VirtualAsm
module AS = Asmx86

module RagMap =
  struct
    type reg
    type t = (Id.t * reg) list
    let del_id : t -> Id.t -> t = fun map id ->
      List.filter (function i, r -> i <> id) map
    let del_reg : t -> reg -> t = fun map reg ->
      List.filter (function i, r -> r <> reg) map
    let id_to_reg (map : t) id =
      match
	List.filter (function i, r -> i = id) map
      with
	| [] -> None
	| (i, r) :: l -> Some r
    let reg_to_id (map : t) reg =
      match
	List.filter (function i, r -> r = reg) map
      with
	| [] -> None
	| (i, r) :: l -> Some i

    let add_map : t -> Id.t -> reg -> t = fun map id reg ->
      (id, reg) :: (del_reg (del_id map id) reg)
  end
