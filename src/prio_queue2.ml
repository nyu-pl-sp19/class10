module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type PrioQueueType =
  sig
    type prio
    type 'a prio_queue
    
    exception Empty
    
    val empty : 'a prio_queue
    val is_empty : 'a prio_queue -> bool
    val insert : 'a prio_queue -> prio -> 'a -> 'a prio_queue
    val min : 'a prio_queue -> 'a option
    val min_prio : 'a prio_queue -> prio
    val delete_min : 'a prio_queue -> 'a prio_queue
  end
      
module MakePrioQueue (O: OrderedType) : PrioQueueType with type prio = O.t =
  struct
    type prio = O.t
    type 'a prio_queue = (prio * 'a) list

    let compare = O.compare
          
    exception Empty

    let empty = []
    let is_empty q =
      q = empty

    let rec insert q p v =
      match q with
      | [] -> [(p, v)]
      | (p1, v1) :: q1 ->
          if compare p p1 < 0 (* p < p1 ? *)
          then (p, v) :: q
          else (p1, v1) :: insert q1 p v

    let min = function
      | [] -> None
      | (_, v) :: _ -> Some v

    let min_prio = function
      | [] -> raise Empty
      | (p, _) :: _ -> p

    let delete_min = function
      | [] -> raise Empty
      | _ :: q1 -> q1

  end

module O =
 struct
   type t = int
   let compare = compare
 end


module PrioQueue = MakePrioQueue(struct
  type t = int
  let compare = compare
end)

module PQ = MakePrioQueue(O)

let q = PrioQueue.empty 

let q1 = PrioQueue.insert q 1 "hello"

let p = PrioQueue.min_prio q
