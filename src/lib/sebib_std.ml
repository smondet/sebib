
include Printf
include Yaboon_PolyComp.CompAndOveridePoly

let (<@>) x y = compare x y <> 0
let (<$>) x y = String.compare x y <> 0


module Ls = struct

    include ExtList.List
    include ListLabels

    let find_opt f l =
        try Some (List.find f l) with Not_found -> None
                      

end
