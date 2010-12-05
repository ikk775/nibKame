type t = Syntax.t list

let stm_read : char Stream.t -> Sexpr.t list = fun stm -> 
  let f stm =
    try
      Some (Sexpr.read stm)
    with
      | Stream.Failure -> None
  in
  let rec g es =
    match f stm with
      | None -> es 
      | Some e -> g (e :: es)
  in
  List.rev (g [])

let read : char Stream.t -> Syntax.t list = fun stm -> 
  List.map Sread.change (stm_read stm)