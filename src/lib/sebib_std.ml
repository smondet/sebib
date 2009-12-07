
include Printf
include Yaboon_PolyComp.CompAndOveridePoly


module Ls = struct

    include ExtList.List
    include ListLabels

    let find_opt f l =
        try Some (List.find f l) with Not_found -> None
                      

end
