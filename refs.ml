(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                 Section 1: Mutable Lists and Cycles
                             Spring 2017
 *)


(* The type of mutable lists. *)
type 'a mlist = Nil
              | Cons of 'a * 'a mlist ref

(*......................................................................
Problem 1: Write a function has_cycle that returns whether a mutable
list has a cycle. You may want a recursive helper function. Don't
worry about space usage.
......................................................................*)

let rec mmem (a: 'a mlist ref) (lst: 'a mlist list) = 
  match lst with 
  | [] -> false
  | h::t -> (h == !a) || (mmem a t)
;;

let has_cycle (lst : 'a mlist) : bool =
  let rec cycle_test (a: 'a mlist list) (lst: 'a mlist) = 
    match lst with
    | Nil -> false
    | Cons (_, t) -> 
      (mmem t a) || (cycle_test (lst::a) !t)
  in 
  cycle_test [] lst
;;

(*......................................................................
Problem 2: Write a function flatten that flattens a list (removes its
cycles if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space.
......................................................................*)
let flatten (lst : 'a mlist) : unit =
  let rec find_cycle (a: 'a mlist list) (lst: 'a mlist) = 
    match lst with 
    | Nil -> ()
    | Cons (_, t) -> 
      if (mmem t (lst::a)) then t:= Nil
      else find_cycle (lst::a) !t
  in 
  find_cycle [] lst
;;
                            
  

(*......................................................................
Problem 3: Write mlength, which nondestructively finds the number of
nodes in a mutable list that may have cycles.
......................................................................*)
let mlength (lst : 'a mlist) : int =
  let rec finder (a: 'a mlist list) (n: int) (lst: 'a mlist) = 
    match lst with 
    | Nil -> n
    | Cons (_, t) -> 
      if (mmem t (lst::a)) then n + 1
      else finder (lst::a) (n + 1) (!t)
  in 
  finder [] 0 lst
;;
        
                         
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = failwith "not provided" ;;
