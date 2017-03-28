(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                      Section 2: Lazy Evaluation
                             Spring 2017
 *)
(*======================================================================
Section 2.1: Series acceleration with infinite streams

In nativeLazyStreams.ml, we provide the definitions of lazy streams
using OCaml's native Lazy module as presented in lecture, up to and
including code for approxiating pi through partial sums of the terms
in a Taylor series. In the next problem, you'll use streams to find
faster approximations for pi.

Recall from lecture the use of streams to generate approximations
of pi of whatever accuracy. Try it. You should be able to reproduce
the following:

   # within 0.01 pi_sums ;;
   - : int * float = (199, 3.13659268483881615)
   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within .001 of the value of pi.  This method converges quite
slowly. But we can increase the speed dramatically by averaging
adjacent elements in the approximation stream.
......................................................................*)

open NativeLazyStreams ;;

(*......................................................................
Problem 5: Implementing average on streams

Write a function average that takes a float stream and returns a
stream of floats each of which is the average of adjacent values in
the input stream. For example:

# first 5 (average (to_float nats)) ;;
- : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
......................................................................*)
  
let rec average (s : float stream) : float stream =
  smap2 (fun x y -> (x +. y) /. 2.0) s (tail s)
;;

(*......................................................................
Problem 6: Implementing Aitken's method

Now instead of using the stream of approximations in pi_sums, you can
instead use the stream of averaged pi_sums, which converges much more
quickly. Test that it requires far fewer steps to get within, say,
0.001 of pi.

An even better accelerator of convergence for series of this sort
is Aitken's method. The formula is given in the problem set
writeup. Write a function to apply this accelerator to a stream, and
use it to generate approximations of pi.
......................................................................*)
   
let aitken (s: float stream) : float stream =
  smap2 (-.) s
    (smap2 (/.)
      (smap (fun x -> x *. x)
        (smap2 (-.) s (tail s)))
      (smap2 (+.)
        (smap2 (-.) s
          (smap (( *. ) 2.) (tail s)))
        (tail  (tail s)))) ;;

(*......................................................................
Problem 7: Testing the acceleration

Fill out the following table, recording how many steps are needed to
get within different epsilons of pi.

    ---------------------------------------------------------
    epsilon  |  pi_sums  |  averaged method  |  aitken method 
    ---------------------------------------------------------
    0.1      |           |                   |
    ---------------------------------------------------------
    0.01     |           |                   |
    ---------------------------------------------------------
    0.001    |           |                   |
    ---------------------------------------------------------
    0.0001   |           |                   |
    ---------------------------------------------------------
......................................................................*)

(*======================================================================
Section 2.2 : Infinite trees

Just as streams are a lazy form of list, we can have a lazy form of
trees. In the definition below, each node in a lazy tree of type 'a
tree holds a value of some type 'a, and a (conventional, finite) list
of one or more (lazy) child trees. Complete the implementation by
writing print_depth, tmap, tmap2, and bfenumerate.  We recommend
implementing them in that order.
......................................................................*)
   
type 'a treeval = Node of 'a * 'a tree list
 and 'a tree = 'a treeval Lazy.t ;;

(* Infinite trees shouldn't have zero children. This exception is
available to raise in case that eventuality comes up. *)

exception Finite_tree ;;

(*......................................................................
Problem 8: Implementing infinite trees
*)

(*......................................................................
node t -- Returns the element of type 'a stored at the root node of
tree t of type 'a tree.
......................................................................*)
let node (t : 'a tree) : 'a =
  let Node (n, _) = Lazy.force t in n
;;

(*......................................................................
children t -- Returns the list of children of the root node of tree t.
......................................................................*)
let children (t : 'a tree) : 'a tree list =
  let Node (_, c) = Lazy.force t in c
;;

(*......................................................................
print_depth n indent t -- Prints a representation of the first n
levels of the tree t indented indent spaces. You can see some examples
of the intended output of print_depth below.
......................................................................*)
let rec print_depth (n : int) (indent : int) (t : int tree) : unit =
  let Node(p, c) = Lazy.force t in
    if (n <= 0) then Printf.printf "%s%d...\n" (Bytes.make indent ' ') p
    else 
      (Printf.printf "%s%d...\n" (Bytes.make indent ' ') p;
       ignore (List.map (fun t -> print_depth (n - 1) (indent + 1) t;
                                   print_newline;) c))
;;


(*......................................................................
tmap f t -- Returns a tree obtained by mapping the function f over
each node in t.
......................................................................*)
let rec tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  let Node (p, c) = Lazy.force t in 
  lazy (Node((f p), List.map (tmap f) c))
;;

(*......................................................................
tmap2 f t1 t2 -- Returns the tree obtained by applying the function f
to corresponding nodes in t1 and t2, which must have the same
"shape". If they don't an Invalid_argument exception is raised.
......................................................................*)
let rec tmap2 (f : 'a -> 'b -> 'c)
              (t1 : 'a tree) (t2 : 'b tree)
            : 'c tree =
  let Node(h1, t1'), Node(h2, t2') = Lazy.force t1, Lazy.force t2 in 
  lazy(Node((f h1 h2), (List.map2 (tmap2 f) t1' t2')));;

(*......................................................................
bfenumerate tslist -- Returns a LazyStreams.stream of the nodes in the
list of trees tslist enumerated in breadth first order, that is, the
root nodes of each of the trees, then the level one nodes, and so
forth. There is an example of bfenumerate being applied below.
......................................................................
 *)
let rec bfenumerate (tslist : 'a tree list) : 'a NativeLazyStreams.stream =
  match tslist with 
  | [] -> raise Finite_tree
  | h::t -> 
    let Node(p, c) = Lazy.force h in 
    lazy (Cons(p, bfenumerate (t @ c)))
;;
  

(* Now use your implementation to generate some interesting infinite
trees.  Hint: Drawing a tree considering how the values change along
each branch will yield helpful intuition for the next problems. *)

(*......................................................................
onest -- An infinite binary tree all of whose nodes hold the integer 1.
......................................................................*)
let rec onest :int tree = lazy(Node(1,[onest; onest]));;
  

(*......................................................................
levels n -- Returns an infinite binary tree where the value of each
node in the tree is its level or depth in the tree, starting with the
argument n. For example:

# print_depth 2 0 (levels 0) ;;
0...
 1...
  2...
  2...
 1...
  2...
  2...
- : unit = ()
......................................................................*)
let rec levels (n : int) : int tree =
        lazy(Node(n, [levels (n + 1); levels (n + 1)]));;

(*......................................................................
Define an infinite binary tree tree_nats where the value of each node in
the tree is consecutively numbered in breadth-first order starting
with 0. For example:

# print_depth 2 0 nats ;;
0...
 1...
  3...
  4...
 2...
  5...
  6...
- : unit = ()
# first 10 (bfenumerate [nats]) ;;
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
......................................................................*)
let rec helper (n: int) : int tree = 
  lazy(Node(n, [helper (2*n + 1); helper (2*n + 2)]));;

let rec tree_nats : int tree =
        helper 0;;

                                                 
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = failwith "not provided" ;;
