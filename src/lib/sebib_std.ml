
include Printf
include Yaboon_PolyComp.CompAndOveridePoly

let (<@>) x y = compare x y <> 0
let (<$>) x y = String.compare x y <> 0


module Ls = struct

    include ListLabels

    let mapi f l =
        let cpt = ref (-1) in
        List.map (fun x -> incr cpt; f !cpt x) l

    let find_opt f l =
        try Some (List.find f l) with Not_found -> None
                      

end
