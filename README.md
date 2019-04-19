# Class 10

## Dealing with Complexity

> There are two ways of constructing a software design: one way is to
> make it so simple that there are *obviously* no deficiencies, and
> the other is to make it so complicated that there are no *obvious*
> deficiencies.

  Tony Hoare

> Computing is the only profession in which a single mind is obliged
> to span the distance from a bit to a few hundred megabytes, a ratio
> of 1 to 10^9, or nine orders of magnitude.  Compared to that number
> of semantic levels, the average mathematical theory is almost flat.
> By evoking the need for deep conceptual hierarchies, the automatic
> computer confronts us with a radically new intellectual challenge
> that has no precedent in our history.

  Edsger Dijkstra

> Software's Primary Technical Imperative has to be managing
> complexity.

  Steve McConnell


A critical aspect of software design is *problem decomposition*, which
is to minimize the amount of essential complexity that has to be dealt
with at any one point in time.  In most cases, this is the *top
priority* when designing software.

An important tool in managing complexity and achieving problem
decomposition is *information hiding*, which is to encapsulate
complexity so that it is not accessible outside of a small part of the
program. This technique has several benefits:

* Helps to decomposes large software systems into small reuusable components

* Safeguards integrity of data

* Helps to compartmentalize run-time errors

* Reduces risk of name conflicts


## Modules

A *module* is a programming language construct that enables problem
decomposition, information hiding, and (often) separate compilation.

A module

* defines a set of logically related entities (*strong internal coupling*)

* has a *public interface* that defines entities exported by the component

* may include other (private) entities that are not exported
  (*information hiding*)

* may depend on the entities defined in the interface of other
  components (*weak external coupling*)

Conceptually, a module is somewhat like a record, but with an
important distinction: 

* A record consists of a set of names called *fields*, which refer to
  values in the record.

* A module consists of a set of names, which can refer to values,
  types, subroutines, other language-specific entities, and possibly other
  modules.

Different languages use different terms for this concept and the
semantics of the corresponding language constructs can differ between
languages (sometimes significantly). Examples: 

* Ada: packages

* C: header files

* C++: header files, classes, namespaces

* Java: interfaces, classes, and packages

* Scala: traits, classes, (package) objects, and packages

* OCaml: modules and functors

Important questions in the design of a module system for a programming
language include:

* How are the public interface and private implementation specified?

* How are dependencies between modules resolved?

* How do the interfaces of similar modules relate? Is there a notion of
  module types?

* What are the naming conventions of imported entities?

* What is the relationship between modules and source code files?

* How does a module control whether a client can access its contents?

* Must names be explicitly imported from outside modules (*closed
  modules*) or are they accessible by default (*open modules*).

* Can modules parameterize over other modules?

We will explore two points in this design space over the next three
weeks (OCaml modules, today), and Scala's classes and objects (next
week and the week after that).

## OCaml's Module System

OCaml's module system consists of a second language layer that sits on
top of OCaml's language for program expressions (for defining values,
which includes functions). An OCaml program is structured into a set of
modules that may depend on each other. Each module consists of a
module signature and a module implementation.

### Module Signatures

The public interface of a module is called a *signature* or *module
type*. It consists of a sequence of declarations of module members,
Module members can be types (including exception types), values, and
nested modules.

As an example, suppose we want to provide a module that implements
FIFO queues. Such queues support the following operations:

* create an empty queue

* check whether a given queue is empty

* enqueue an element into a queue, yielding a new queue with the added
  element

* dequeue an element from the queue, yielding the dequeued element and
  a new queue with that element removed.

```ocaml
module type QueueType =
  sig
    type 'a queue
    val empty : 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a -> 'a queue -> 'a queue
    val dequeue : 'a queue -> ('a * 'a queue) option
  end
```

Note that the signature does not specify what the type `'a queue` is
(i.e. how it is implemented). It only specifies that this type exists
and that it is parametric in some other type `'a`.

Also note that the name of the module type, here `QueueType`, must
start with a capital letter.

### Module Structures

The actual implementation of a module is given by a *module
structure*. The module structure provides the definitions for all the
members declared in the module signature. A module structure can have
more members than those declared in a module signature, though it must
at least provide definitions for all the members of the signature and
the types of those definitions must be consistent with the types
provided in the signature.

For example, the following module structure `Queue` implements the
signature `QueueType` by choosing to represent the type `'a
queue` of `QueueType` by the concrete type `'a list`.

```ocaml
module Queue : QueueType =
  struct
    type 'a queue = 'a list
    exception Empty
        
    let empty = []

    let is_empty q =
      q = empty

    let enqueue x q =
      List.append q [x]
        
    let dequeue = function
      | [] -> None
      | x :: q -> Some (x, q)
  end
```

The syntax

```ocaml
module Queue : QueueType = struct
  ...
end
```

indicates that we declare a new module structure, called `Queue`, that
implements the module signature `QueueType`. The actual implementation
of the signature is provided within the `struct ... end` block. This
block may consist of any number of member definitions (i.e. types,
exceptions, values, and submodule), but it must implement at least all
the members declared in `QueueType`. The members can be defined in any
order and the order does not need to be consistent with the order how
they are declared in the signature. Note that similar to module
signatures, the names of modules must always start with a capital
letter. There can be any number of module structures implementing the
same module signature.


Once a module structure `M` has been defined, we can refer to a
member `x` declared in its signature using the notation `M.x`. For
instance, here is how we can use our `Queue` module to manipulate FIFO
queues:

```ocaml
let q = Queue.empty

let q1 = Queue.enqueue 1 q

let q2 = Queue.enqueue 2 q1

let v1, q3 = Queue.dequeue q2 |> Opt.get

let v2, q4 = Queue.dequeue q3 |> Opt.get

let _ = Printf.printf "%d %d\n" v1 v2 // prints "1 2"
```

Equality on the values of the type `'a queue` is determined by the
equality defined on the underlying implementation type. For instance,
with the above definitions, we will have that the expression `q = q4`
will evaluate to `true` because after dequeuing twice from the queue
`q2`, which contains two elements, we again obtain an empty queue.


#### Opening Modules

Prefixing every module member with the name of the module can be
cumbersome, in particular if we want to access members of modules that
are nested within other modules. OCaml allows to import all members of
a module into the current scope by *opening* the module. At the
top-level of a module implementation, we can open another module `M`,
using the `open` directive:

```ocaml
open M
```

For instance, the code above for creating and manipulating FIFO queues
could also be written like this

```ocaml
open Queue

let q = empty

let q1 = enqueue 1 q

let q2 = enqueue 2 q1

let v1, q3 = dequeue q2 |> Opt.get

let v2, q4 = dequeue q3 |> Opt.get

let _ = Printf.printf "%d %d\n" v1 v2 // prints "1 2"
```

Be aware that opening a module means that any value that was
accessible in the current scope before the open directive and that has
the same name as one of the members of the module being opened will be
shadowed by the module's member. That is, if we have some value `x` in
a scope where we open a module `M` that also defines a member `x`,
then any reference to `x` after the open directive will refer to
`M.x`. As we shall see below, all non-local values belong to some
module. So we can always disambiguate between different values that
have the same name `x` by prefixing the value with a qualifier `M.x` that
explicitly indicates the module that it belongs to (here `M`).

Also, to avoid "pollution" of the top-level scope of a module with
names imported from other modules, OCaml supports several forms of
opening modules locally within a nested scope. For instance, the
following code opens the module `Queue` only in the local scope of the
defining expression of `q`:

```ocaml
let q =
  let open Queue in
  enqueue 2 (enqueue 1 empty)
```

The same effect can be achieved using the syntax `M.(e)` which opens
module `M` only within the scope of the expression `e`. That is, the
above code can also be written as

```ocaml
let q = Queue.(enqueue 2 (enqueue 1 empty))
```

### Information Hiding

One important aspect of module signatures is that they enable
information hiding. A client of a module `M` that implements a
specific module signature `S` can only rely on the information
provided by the signature `S`. If a module implementation defines
members that are not part of its signature, then those members are not
accessible by clients. Similarly, only the information about the
module's types that is provided in the signature is visible to the
client.

For instance, note that our signature `QueueType` only declares the
type `'a queue`. It does not specify how this type is
implemented. This means that the details of how this type is
implemented in the module `Queue` are opaque to the client code that
uses the module. In particular, client code cannot rely on the fact
that this type is implemented as `'a list`. For example, the following
client code for printing a queue will not compile because it relies on
the fact that `'a Queue.queue` is implemented as a list:

```ocaml
let print_int_queue (q: int Queue.queue) =
  let rec pr q = match q with
  | [] -> ()
  | x :: q1 -> print_int x; print_string ", "; pr q1
  in 
  pr q
```

Instead, this code would have to written in terms of the members of
`QueueType` for manipulating the queue values:

```ocaml
let print_int_queue (q: int Queue.queue) =
  let rec pr q = match dequeue q with
  | None -> ()
  | Some (x, q1) -> print_int x; print_string ", "; pr q1
  in
  pr q
```

This is an important feature of modules, as it creates a barrier
between the implementation of a module and its client code that
prevents the client code from breaking when the implementation of the
module changes. This is critical for the design and maintenance of
large code bases as changes to the code base should be insulated from
affecting large portions of the code.

As a motivating example, suppose that we have a large system that uses
our `Queue` module from above in many places. Note that the
implementation of `Queue` is actually not very efficient: the
`enqueue` operation uses `List.append` to add the new element `x` at
the end of the queue `q`. This operation is linear in the length of
`q`. However, we would expect that both `enqueue` and `dequeue`
operations run in constant time - at least amortized over all
operations performed on the queue during execution of the program. So
let's try to make our implementation of the queue data structures and
see whether we can do this in a way such that remaining code of the
larger system that uses the `Queue` module does not need to be
modified.

We can improve our implementation of `Queue` by using a pair of two
lists `(q, r)` to represent the queue contents rather than a single
list. The first list `q` is used by dequeue operations the same way as
before, i.e. to dequeue an element, we remove it from the front of
`q`. The second queue `r` is used by enqueue operations, i.e. to
enqueue an element, rather than adding it to the back of `q` we now
add it to the front of `r`. That is, at any time, the actual queue
contents is represented by the list `List.append q (List.rev r)` where
`List.rev` reverses a list.

If we want to dequeue from a queue whose `q` component is empty but
whose `r` component is non-empty, then we simply set the `q` component
of the queue to `List.rev r` and set the `r` component to the empty
list. Then we can continue dequeuing from the front of the new `q`
component. The following module implementation realizes this idea:


```ocaml
module Queue : QueueType =
  struct
    type 'a queue = 'a list * 'a list

    let empty =
      [], []

    let is_empty q =
      empty = q

    let enqueue x (q, r) =
      q, x :: r
    
    let rec dequeue = function
      | [], [] -> None
      | [], r -> dequeue (List.rev r, [])
      | x :: q, r -> Some (x, (q, r))
  end
```

Adding or removing an element from the front of a list can be done in
constant time. The cost of executing `List.rev r` in `dequeue` is
linear in the size of the `r` component. However, if `r` contains `n`
elements when `List.rev r` is executed in a call to `dequeue`, then
there must have been `n` preceding `enqueue` operations that built `r`
up to length `n`. Hence, the cost of executing `List.rev` can be
"distributed" over all these `n` `enqueue` operations, which only
increases the cost of each of those operations by a constant. Hence,
both `enqueue` and `dequeue` now run in amortized constant time.

Now, let's revisit our client code for printing the queue. Note that
client code that relied on the queue being implemented as a list would
be broken by the changes to the list module (because a queue is now
represented as a pair of lists rather than a single list). However,
the code that prints the queue by only interacting with the queue
using the members provided by the `QueueType` signature will still
work as before.

Sometimes we want to expose the implementation of a type member of a
module signature to the client code. In those cases, we can simply
make the definition of the type member public in the type
signature. For instance, if we wanted to allow client code to have
direct access to the underlying list implementation of our original
`Queue` module, we could change the signature `QueueType` to expose
the definition of type `'a queue`:

```ocaml
module type QueueType =
  sig
    type 'a queue = 'a list (* exposes implementation of type 'a queue *)
    val empty : 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a -> 'a queue -> 'a queue
    val dequeue : 'a queue -> ('a * 'a queue) option
  end
```

Note also that it is not obligatory to define module signatures. For
instance, we could implement our `Queue` module without specifying the
signature that the module implements:

```ocaml
module Queue =
  struct
    type 'a queue = 'a list * 'a list
    let empty =
      [], []

    let is_empty q =
      empty = q

    let enqueue x (q, r) =
      q, x :: r
    
    let rec dequeue = function
      | [], [] -> None
      | [], r -> dequeue (List.rev r, [])
      | x :: q, r -> Some (x, (q, r))
  end
```

If the module signature is omitted in a module definition, then the
compiler will automatically infer the signature by performing type
inference on the definitions of the module members. However, note that
the inferred module signature will be maximally permissive and not
enforce any information hiding. That is, all members declared in the
module implementation will be accessible by the clients of the module
and all the type definitions will be exposed to the client code.

### Modules and Separate Compilation

OCaml also uses modules to realize *separate compilation*. Each source
code file `foo.ml` of an OCaml program implicitly defines a module of
name `Foo`. That is, in general, the name of the module associated
with a source code file is the capitalized name of the file, where the
file extension `.ml` is omitted. A member `x` declared in file
`foo.ml` can then be accessed in another source code file `bar.ml` by
using `Foo.x`. Also, we can import the members of a source code file
into another source code file by using module open directives as
explained earlier. 


The OCaml compiler compiles the source code files one at a time. If a
source code file `bar.ml` refers to a member defined in module
`foo.ml`, then `foo.ml` is compiled before `bar.ml`. This also means
that if `bar.ml` depends on `foo.ml`, then `foo.ml` cannot in turn
also depend on `bar.ml`; the top-level module dependency graph must be
acyclic. OCaml allows cyclic dependencies using [recursive module
definitions](https://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec236).
However, in this case the two interdependent modules need to be
defined in a single source code file.

The signature of the implicit module `Foo` associated with a source
code file `foo.ml` can be defined in the dedicated signature file
`foo.mli`.

For instance, suppose that we defined the module `Queue` and its
signature `QueueType` in a source code file `queue.ml`. Then, e.g.,
the member `empty` of the queue module could be accessed in other
source code files using `Queue.Queue.empty`. This is because the
module `Queue` is a nested module within the implicit module `Queue`
defined by `queue.ml`. This notation is a bit cumbersome. If
`queue.ml` only contains the implementation of the queue module, then
we can eliminate the nested module and instead define the members of
the queue module directly at the top-level of `queue.ml`:

```ocaml
(** queue.ml *)
type 'a queue = 'a list * 'a list

let empty =
  [], []

let is_empty q =
  empty = q

let enqueue x (q, r) =
  q, x :: r
    
let rec dequeue = function
  | [], [] -> None
  | [], r -> dequeue (List.rev r, [])
  | x :: q, r -> Some (x, (q, r))
```

In addition, we can provide the module signature of the module defined by
`queue.ml` in thean associated file called `queue.mli`:

```ocaml
(** queue.mli *)

type 'a queue

val empty : 'a queue

val is_empty : 'a queue -> bool

val enqueue : 'a -> 'a queue -> 'a queue

val dequeue : 'a queue -> ('a * 'a queue) option
```

This way, we can e.g. access `empty` in other source code files using
just `Queue.empty`. Moreover, the module signature still ensures that
the implementation of type `'a queue` is hidden from code that uses
`Queue` in other source code files.


### Higher-Order Modules: Functors

The true power of OCaml's module system stems from the fact that it
allows module implementations to parameterize over the implementation
of other modules. That is, modules can take other modules as
parameters. Such modules can be viewed as higher-order modules and are
referred to as *functors* (the name refers to the notion of a functor
in category theory).

To motivate the use of functors, suppose we want to implement a simple
priority queue data structure. A priority queue stores pairs `(p, v)`
of values `v` and an associated priority `p` drawn from some totally
ordered set.

A priority queue provides the following operations:

* create an empty priority queue,

* insert a new pair `(p, v)` into the queue,

* retrieve the element `v` with the smallest priority from the queue, and

* remove the element with the smallest priority from the queue.

This data structure is crucial for many algorithms (e.g. Dijkstra's
algorithm for computing the shortest paths in a graph).

The following module signature declares the types of the priority
queue operations. We make the implementation parametric in the type of
values `v` stored in the queue. However, for now we fix the type of
priorities to be `int`, which is captured by the type `prio` in the
module signature:

```ocaml
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
```

Below is a simple (though not very efficient) implementation of this
module signature. We use a list of pairs of priorities and values as
the representation of the priority queue. The module maintains the
invariant that the pairs `(p, v)` are stored in increasing order
according to the ordering on the priority values. Thus, `min` and
`delete_min` simply retrieve respectively remove the first element of
the list. The implementation of `insert` uses the predefined OCaml function

```ocaml
val compare: 'a -> 'a -> int
```

which defines a total ordering on every OCaml type `'a`. Specifically,
`compare x y` returns a negative integer if `x` is smaller than `y`, a
positive integer if `x` is greater than `y`, and `0` if `x` and `y`
are equal. The implementation of `compare` chooses an appropriate
total ordering for each type `'a`. For the type `int`, the ordering
implemented by `compare` is the standard ordering on integers.

Here is the implementation:

```ocaml
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
            then (p, v) :: q
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
```

So far so good, but what if we wanted to make our implementation
parametric in both the type of the values stored in the queue as well
as the type of priorities and their associated comparison function
defining the ordering? A first idea would be to change our underlying
representation of priority queues so that it does not just store the
actual queue, but also the comparison function defining the ordering
on the queue. With this approach, the value `empty` becomes a function
that takes in the comparison function and creates an empty queue from
it. Here is how this could look like:

```ocaml
module type PrioQueueType =
  sig
    (* Define alias for type of compare functions *)
    type 'prio compare_fun = 'prio -> 'prio -> int 
    
    (* Priority queue type is now parametric in the type of priorities
      'prio as well as the associated values 'a *)
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
      ins q, compare
      
    let min = function
      | [], _ -> None
      | (_, v) :: _, _ -> Some v

    let delete_min = function
      | [], _ -> raise Empty
      | _ :: q1, compare -> (q1, compare)
  end
```

We can now create priority queues for different priority types and comparison functions:

```ocaml
(** priority queue with int priorities ordered increasingly *)
let q1: (int, 'a) queue = PrioQueue.empty compare

(** priority queue with int priorities ordered decreasingly *)
let q2: (int, 'a) queue = PrioQueue.empty (fun x y -> - (compare x y))

(** priority queue with string priorities ordered increasingly *)
let q3: (string, 'a) queue = PrioQueue.empty compare
```

While this is an OK solution, it has one deficiency: the queues `q1`
and `q2` have the same types even though they work quite differently:
`q2` uses the inverse priority ordering of `q1`. If we had a program
that used both of these queues simultaneously, the type checker would be
unable to detect bugs related to accidentally using `q1` instead of
`q2` in the program (and vice versa). It would be nice if we could
implement the module `PrioQueue` in a way that allows us to
distinguish between the two queues at the level of the type system, so
that the kinds of bugs above were detected at compile time.

This is where functors come into play. Essentially, functors allow us
to parameterize the `PrioQueue` module over another module that
implements the ordering on the priority type. Applying this functor to
different priority type modules yields different priority queue
modules that have distinct `prio_queue` types.


First, let's write a module signature for priority types. The module
signature simple declares an abstract type `t` representing the type
of priority values as well as the associated `compare` function:

```ocaml
module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end
```

Next, let's define the module signature for the priority queue modules
created by the functor:

```ocaml
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
```

Note that just like the `prio_queue` type, the type `prio` is now
opaque: we only specify that it exists but we don't say how it is
implemented.

Then we can define the functor for creating priority queues as follows:

```ocaml
module MakePrioQueue (O: OrderedType) : PrioQueueType =
  struct
    type prio = O.t
    type 'a prio_queue = (prio * 'a) list

    let compare = O.compare
          
    exception Empty

    let empty = []
    let is_empty q =
      q = empty

    let insert q p v =
      let rec ins = function
        | [] -> [(p, v)]
        | (p1, v1) :: q1 ->
            if compare p p1 < 0 (* p < p1 ? *)
            then (p, v) :: q
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
```

The line

```ocaml
module MakePrioQueue (O: OrderedType) : PrioQueueType = ...
```

Defines the functor `MakePrioQueue` that takes in as argument a module
`O` implementing the signature `OrderedType` and returns another
module implementing the type `PrioQueueType`. Note that the body of
the functor implements the type `prio` using the type `O.t` of the
ordered type. Similarly, we use the comparison function `O.compare` on
the ordered type to determine the ordering of the elements in the queue.

We can now use the functor to create different priority queue modules
for different ordered types:

```ocaml
(** Module for int values ordered increasingly *)
module IntIncreasing : OrderedType = struct
  type t = int
  let compare = compare
end

(** Create priority queue module over IntIncreasing *)
module IntIncPrioQueue = MakePrioQueue(IntIncreasing)

(** Module for int values ordered decreasingly *)
module IntDecreasing : OrderedType = struct
  type t = int
  let compare x y = - (compare x y)
end

(** Create priority queue module over IntDecreasing *)
module IntDecPrioQueue = MakePrioQueue(IntDecreasing)

let q1 = IntIncPrioQueue.empty

let q2 = IntDecPrioQueue.empty
```

The two values `q1` and `q2` now belong to two different types
(`IntIncPrioQueue.prio_queue` respectively
`IntDecPrioQueue.prio_queue`) and hence they can now be
distinguished at the level of the type system.

Note that we can also inline the definition of the module structure that provides the ordered type to the functor:

```ocaml
module IntIncPrioQueue = MakePrioQueue(struct
  type t = int
  let compare = compare
end)
```

#### Establishing type equalities in functor definitions

One problem with the implementation of our functor is that, while we can create an empty priority queue, we can't actually insert any elements into it:

```ocaml

let p = IntIncPrioQueue.empty

let p1 = IntIncPrioQueue.insert p 1 "apple"

Error: This expression has type int but an expression was expected of type
         IntIncPrioQueue.prio = MakePrioQueue(IntIncreasing).prio
```

The problem is that because the implementation of type `prio` is
hidden by the module signature `PrioQueueType`, once we have applied
the functor, we lose the information that the type `IntIncreasing.t`
which is `int` is equal to the type `IntIncPrioQueue.prio`. The
information that these two types are actually the same is internal to
the implementation of the functor but hidden from the client by the
signatures.

If we want to expose a type equality between two types in the module
parameter of a functor and the module created by the functor, we have
to make this equality explicit in the functor definition. In our
example, this can be done using the following syntax:


```ocaml
module MakePrioQueue (O: OrderedType) : PrioQueueType with type prio = O.t = ...
```

The constraint `PrioQueueType with type prio = O.t` modifies the type
signature PrioQueueType by exposing the implementation of the type
`prio`. With this new version of the functor, we can now insert
elements into our queue as expected:

```ocaml
let p = IntIncPrioQueue.empty

let p1 = IntIncPrioQueue.insert p 1 "apple"
```
