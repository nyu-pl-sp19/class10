type 'a queue = 'a list * 'a list

exception Empty
        
let empty =
  [], []

let is_empty q =
  empty = q

let enqueue x (q, r) =
  (q, x :: r)
    
let rec dequeue = function
  | [], [] -> raise Empty
  | [], r -> dequeue (List.rev r, [])
  | x :: q, r -> x, (q, r)
