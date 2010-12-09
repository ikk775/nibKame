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
  
  let rec setDiff xs ys =
    List.filter (fun x -> not (List.mem x ys)) xs

  let shuffle a =
    let ary = Array.of_list a in
    Array.shuffle ary;
    Array.to_list ary

  let rec unique = function
    | [] -> []
    | x :: xs -> x :: setDiff (unique xs) [x]
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

exception Undefined
let rec undefined () = raise Undefined