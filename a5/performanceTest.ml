(* All the code in this file is provided just to help you.
   You can change it or ignore it as you see fit. *)

(** [i -- j] is the list of integers from [i] to [j], inclusive.
    Tail recursive. *)
let (--) (i : int) (j : int) : int list =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
  in from i j []

(** [rnd_lst n] is a list of [n] random integers. *)
let rnd_list (n : int) : int list =
  QCheck.Gen.(generate ~n int)

(** [rnd_sort_lst n] is a list of [n] random integers in ascending order. *)
let rnd_sort_list (n : int) : int list =
  QCheck.Gen.(generate ~n int) |> List.sort Stdlib.compare

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle (lst : 'a list) : 'a list =
  QCheck.Gen.(generate1 (shuffle_l lst))

(** [time f] is the time in seconds it takes to run [f ()]. *)
let time (f : unit -> unit) : float =
  let t = Unix.gettimeofday () in
  ignore (f ());
  Unix.gettimeofday () -. t 
