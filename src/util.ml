(** {0} Utility functions *)

(** Utility functions on option types *)
module Opt = struct
  let to_list = function
    | Some x -> [x]
    | None -> []

  let get = function
    | Some x -> x
    | None -> failwith "Opt.get applied to None"

  let get_or_else default = function
    | Some x -> x
    | None -> default

  let lazy_get_or_else f y = function
    | Some x -> x
    | None -> f y

  let or_else y = function
    | Some x -> Some x
    | None -> Some y

  let lazy_or_else f y = function
    | Some x -> Some x
    | None -> f y

  let fold f init = function
    | Some x -> f init x
    | None -> init

  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let flat_map f = function
    | Some x -> f x
    | None -> None
          
  let iter f = function
    | Some x -> f x
    | None -> ()

  let some x = function
    | None -> Some x
    | o -> o
end
