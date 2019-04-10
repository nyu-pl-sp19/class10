type 'a queue = 'a list * 'a list

let empty =
  [], []

let is_empty q =
  empty = q

let enqueue x (q, r) =
  (q, x :: r)
    
let rec dequeue = function
  | [], [] -> None
  | [], r -> dequeue (List.rev r, [])
  | x :: q, r -> Some (x, (q, r))
