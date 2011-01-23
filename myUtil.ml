module X = MyUtilx

module Array = struct
  include Array
  include X.Array
end
module Format = struct
  include Format
  include X.Format
end

module List = struct
  include List
  include X.List
end

module String = struct
  include String
  include X.String
end

exception Undefined
let rec undefined () = raise Undefined

let rec times n f x =
  if n > 0
  then times (n - 1) f (f x)
  else x
