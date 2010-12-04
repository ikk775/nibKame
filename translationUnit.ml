type t = Syntax.t list

let read : char Stream.t -> t = fun stm -> 
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
  List.map Sread.change (g [])
