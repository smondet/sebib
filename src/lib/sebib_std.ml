
include Printf
(*
include Safe_int (* Removes all polymorphic comparisons*)
*)
let (==) (x:unit) (y:unit) = ()

let (=$=) x y = String.compare x y = 0
let (<$>) x y = String.compare x y <> 0

(* Polymorphic equality *)
let (=@=) x y = compare x y = 0
let (<@>) x y = compare x y <> 0

module Ls = struct
    (*include List*)
    include ListLabels

    let mapi f l =
        let cpt = ref (-1) in
        List.map (fun x -> incr cpt; f !cpt x) l

    let find_opt f l =
        try Some (List.find f l) with Not_found -> None
                      
    (*include List.Labels.LExceptionless*)
end
