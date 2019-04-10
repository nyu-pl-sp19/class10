module type PrioQueueType =
  sig
    type 'prio compare_fun = 'prio -> 'prio -> int        
    type ('prio, 'a) prio_queue
    
    exception Empty
    
    val empty : 'prio compare_fun -> ('prio, 'a) prio_queue
    val is_empty : ('prio, 'a) prio_queue -> bool
    val insert : ('prio, 'a) prio_queue -> 'prio -> 'a -> ('prio, 'a) prio_queue
    val min : ('prio, 'a) prio_queue -> 'a option
    val delete_min : ('prio, 'a) prio_queue -> ('prio, 'a) prio_queue
  end

module PrioQueue : PrioQueueType =
  struct
    type 'prio compare_fun = 'prio -> 'prio -> int
    type ('prio, 'a) prio_queue = ('prio * 'a) list * 'prio compare_fun

    exception Empty

    let empty compare =
      ([], compare)

    let is_empty (q, _) =
      q = []

    let insert (q, compare) p v =
      let rec ins q =
        match q with
        | [] -> [(p, v)]
        | (p1, v1) :: q1 ->
            if compare p p1 < 0 (* p < p1 ? *)
            then (p, v) :: q
            else (p1, v1) :: ins q1
      in
      (ins q, compare)
      
    let min = function
      | [], _ -> None
      | (_, v) :: _, _ -> Some v

    let delete_min = function
      | [], _ -> raise Empty
      | _ :: q1, compare -> (q1, compare)
  end
