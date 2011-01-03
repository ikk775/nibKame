open MyUtil

let write_number n =
  Format.sprintf "%ds" n

let read_number stm =
  let rec f cs =
    match Stream.next stm with
      | c when c = 's' -> cs
      | c when String.contains "0123456789" c -> f (c :: cs)
      | c -> invalid_arg "of_string"
  in
  int_of_string (String.implode (List.rev (f [])))

let write_id x =
  assert (Id.is_valid x);
  let len = String.length x in
  Format.sprintf "%ds%s" len x

let read_id stm =
  let cn = read_number stm in
  let rec f n cs =
    if n <= 0
    then cs
    else f (n - 1) (Stream.next stm :: cs)
  in
  String.implode (List.rev (f cn []))

let write_seq f xs =
  let len = List.length xs in
  let strs = List.map f xs in
  let str = String.concat "" strs in
  Printf.sprintf "%ds" len ^ str

let read_seq stm f =
  let xn = read_number stm in
  List.iter_list xn (fun () -> f stm)

