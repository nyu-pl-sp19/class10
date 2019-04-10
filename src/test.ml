open Queue_demo

let q = Queue.empty

let flip f x y = f y x
    
let q1 =
  List.fold_left
    (flip Queue.enqueue)
    Queue.empty
    [1; 2; 3; 4]

let v, _ = Queue.(empty |> enqueue 1 |> enqueue 2 |> dequeue)

let _ = Printf.printf "%d\n" v
