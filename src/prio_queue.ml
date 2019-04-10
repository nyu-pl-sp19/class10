module type PrioQueueType =
  sig
    type prio = int
    type 'a prio_queue
    
    exception Empty
    
    val empty : 'a prio_queue
    val is_empty : 'a prio_queue -> bool
    val insert : 'a prio_queue -> prio -> 'a -> 'a prio_queue
    val min : 'a prio_queue -> 'a option
    val delete_min : 'a prio_queue -> 'a prio_queue
  end

module PrioQueue : PrioQueueType =
  struct
    type prio = int
    type 'a prio_queue = (prio * 'a) list

    exception Empty

    let empty = []
    let is_empty q =
      q = empty

    let insert q p v =
      let rec ins = function
        | [] -> [(p, v)]
        | (p1, v1) :: q1 ->
            if compare p p1 < 0 (* p < p1 ? *)
            then (p, v) :: (p1, v1) :: q1
            else (p1, v1) :: ins q1
      in
      ins q

    let min = function
      | [] -> None
      | (_, v) :: _ -> Some v

    let delete_min = function
      | [] -> raise Empty
      | _ :: q1 -> q1
  end
