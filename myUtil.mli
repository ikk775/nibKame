module Array :
  sig
    val swap :  'a array -> int -> int -> unit
    val shuffle :  'a array -> unit
  end
module Format :
  sig
    val call_with_output_string : (Format.formatter -> 'a) -> string
  end
module List :
  sig
    val iota : ?step:int -> int -> int -> int list
    val setDiff : 'a list -> 'a list -> 'a list
    val shuffle :  'a list -> 'a list
  end
module String :
  sig
    val explode : string -> char list val implode : char list -> string
  end
