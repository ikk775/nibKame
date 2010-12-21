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