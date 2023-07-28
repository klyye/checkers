module Coord = struct
  type t = int * int

  let compare a b =
    let ar, ac = a in
    let br, bc = b in
    if ar = br then ac - bc else ar - br
end

module CoordSet = Set.Make (Coord)

let fold_lefti f x a =
  let r = ref x in
  for i = 0 to Array.length a - 1 do
    r := f !r a.(i) i
  done;
  !r
