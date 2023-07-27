module Coord = struct
  type t = int * int

  let compare a b =
    let ar, ac = a in
    let br, bc = b in
    if ar = br then ac - bc else ar - br
end

module CoordSet = Set.Make (Coord)
