
include Print

include Safe_int (* Removes all polymorphic comparisons*)

let (==) (x:unit) (y:unit) = ()

let (=$=) x y = String.compare x y = 0
let (<$>) x y = String.compare x y <> 0

(* Polymorphic equality *)
let (=@=) x y = Standard.compare x y = 0
let (<@>) x y = Standard.compare x y <> 0

module Ls = struct
    include List
    include List.Labels
    include List.Labels.LExceptionless
end
