module Array = struct
  include Array
  let swap a i j =
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
    
  let shuffle a =
    Array.iteri (fun i _ -> swap a i (Random.int (i + 1))) a
end
module Format = struct
  include Format
  let call_with_output_string proc =
    let buf = Buffer.create 127 in
    let buf_f = Format.formatter_of_buffer buf in
    proc buf_f;
    Format.pp_print_flush buf_f ();
    Buffer.contents buf
end

module List = struct
  include List
  let rec iota ?(step = 1) s e =
    if s > e
    then []
    else s :: iota ~step (s + step) e
  
  let rec mem ?(eq = (=)) a = function
    | [] -> false
    | x :: xs ->
      if eq a x
      then true
      else mem ~eq:eq a xs
    
  let rec setDiff ?(eq = (=)) xs ys =
    List.filter (fun x -> not (mem ~eq:eq x ys)) xs

  let shuffle a =
    let ary = Array.of_list a in
    Array.shuffle ary;
    Array.to_list ary

  let rec unique ?(eq = (=)) = function
    | [] -> []
    | x :: xs -> x :: setDiff ~eq:eq (unique xs) [x]

  let rec select = function
    | [] -> []
    | [xs] -> List.map (fun x -> [x]) xs
    | xs :: xss ->
      let tl = select xss in
      List.concat (List.map (fun y -> List.map (fun x -> x :: y) xs) tl)

  let rec iter_list n f =
    let rec g n =
      if n <= 0
      then []
      else (f ()) :: g (n - 1)
    in
    List.rev (g n)
end

module String = struct
  include String
  let explode str =
    let stm = Stream.of_string str in
    let rec explode_sub cs =
      try
        explode_sub ((Stream.next stm) :: cs)
      with
        | Stream.Failure -> cs
    in
    List.rev (explode_sub [])
  let implode cs =
    Format.call_with_output_string (fun fmtr -> 
      List.iter (Format.fprintf fmtr "%c") cs)
end
