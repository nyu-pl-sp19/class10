open Util

module type QueueType =
  sig
    type 'a queue
    val empty : 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a -> 'a queue -> 'a queue
    val dequeue : 'a queue -> ('a * 'a queue) option
  end

module Queue : QueueType =
  struct
    type 'a queue = 'a list * 'a list

    let empty = [], []

    let is_empty q = 
      q = empty 
      
    let enqueue x q = 
      match q with
      | enq, deq -> x :: enq, deq

    let rec dequeue = function
      | [], [] -> None
      | enq, x :: deq -> Some (x, (enq, deq))
      | enq, [] -> dequeue ([], List.rev enq)

  end

let q = Queue.empty

let q1 = Queue.enqueue 1 q

let q2 = Queue.enqueue 2 q1

let v1, q3 = Queue.dequeue q2 |> Opt.get

let v2, q4 = Queue.dequeue q3 |> Opt.get

let q = Queue.(empty |> enqueue 1 |> enqueue 2)


(*let xs = 3 :: q*)

let _ = Printf.printf "%d %d\n" v1 v2


let q = MyQueue.empty
