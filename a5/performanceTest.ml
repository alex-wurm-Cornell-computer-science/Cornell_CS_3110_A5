(* All the code in this file is provided just to help you.
   You can change it or ignore it as you see fit. *)
open Dictionary
open ListDictionary
open DictionarySet
open TreeDictionary

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

(* Add your own code here. *)

module IntKey
  : KeySig with type t = int
=
struct
  type t = int
  let compare i1 i2 =
    match Stdlib.compare i1 i2 with
    | x when x < 0 -> LT
    | x when x > 0 -> GT
    | _ -> EQ
  let format fmt i =
    Format.fprintf fmt "\"%d\"" i
end

(* Random lists *)
let randomlist10k = rnd_list 10000

let randomlist20k = rnd_list 20000

let randomlist30k = rnd_list 30000

let randomlist40k = rnd_list 40000

let randomlist50k = rnd_list 50000

let randomlist60k = rnd_list 60000

let randomlist70k = rnd_list 70000

let randomlist80k = rnd_list 80000

let randomlist90k = rnd_list 90000

let randomlist100k = rnd_list 100000

let randomlist200k = rnd_list 200000

let randomlist300k = rnd_list 300000

let randomlist400k = rnd_list 400000

let randomlist500k = rnd_list 500000

let randomlist600k = rnd_list 600000

let randomlist700k = rnd_list 700000

let randomlist800k = rnd_list 800000

let randomlist900k = rnd_list 900000

let randomlist1M = rnd_list 1000000

(* Ascending lists *)
let sortedlist10k = rnd_sort_list 10000

let sortedlist20k = rnd_sort_list 20000

let sortedlist30k = rnd_sort_list 30000

let sortedlist40k = rnd_sort_list 40000

let sortedlist50k = rnd_sort_list 50000

let sortedlist60k = rnd_sort_list 60000

let sortedlist70k = rnd_sort_list 70000

let sortedlist80k = rnd_sort_list 80000

let sortedlist90k = rnd_sort_list 90000

let sortedlist100k = rnd_sort_list 100000

let sortedlist200k = rnd_sort_list 200000

let sortedlist300k = rnd_sort_list 300000

let sortedlist400k = rnd_sort_list 400000

let sortedlist500k = rnd_sort_list 500000

let sortedlist600k = rnd_sort_list 600000

let sortedlist700k = rnd_sort_list 700000

let sortedlist800k = rnd_sort_list 800000

let sortedlist900k = rnd_sort_list 900000

let sortedlist1M = rnd_sort_list 1000000

module ListDictTest = functor (LD : DictionarySet.Set with 
type Elt.t = IntKey.t) -> struct

  module L = DictionarySet.Make(IntKey)(ListDictionary.Make)
  type t = L.t

   let rec insert_all l d =
    match l with
    | [] -> d
    | h::t -> let d' = L.insert h d in 
              insert_all t d'

    let rec test4 l d =
      match l with
      | [] -> ()
      | h::t -> ignore(L.member h d);
                ignore(L.member h d);
                ignore(L.member h d);
                ignore(L.member h d);
                ignore(test4 t d)

    let time_tests l d =
      let timed_run = fun () -> d |> insert_all l |> test4 l in
      time timed_run

    let run_data =
      Printf.printf "n,List-Asc,List-Rnd\n";
      Printf.printf "10000,%f,%f\n" (time_tests sortedlist10k L.empty)
      (time_tests randomlist10k L.empty);
      Printf.printf "20000,%f,%f\n" (time_tests sortedlist20k L.empty)
      (time_tests randomlist20k L.empty);
      Printf.printf "30000,%f,%f\n" (time_tests sortedlist30k L.empty)
      (time_tests randomlist30k L.empty);
      Printf.printf "40000,%f,%f\n" (time_tests sortedlist40k L.empty)
      (time_tests randomlist40k L.empty);
      Printf.printf "50000,%f,%f\n" (time_tests sortedlist50k L.empty)
      (time_tests randomlist50k L.empty);
      Printf.printf "60000,%f,%f\n" (time_tests sortedlist60k L.empty)
      (time_tests randomlist60k L.empty);
      Printf.printf "70000,%f,%f\n" (time_tests sortedlist70k L.empty)
      (time_tests randomlist70k L.empty);
      Printf.printf "80000,%f,%f\n" (time_tests sortedlist80k L.empty)
      (time_tests randomlist80k L.empty);
      Printf.printf "90000,%f,%f\n" (time_tests sortedlist90k L.empty)
      (time_tests randomlist90k L.empty);
      Printf.printf "100000,%f,%f\n" (time_tests sortedlist100k L.empty)
      (time_tests randomlist100k L.empty);

end

module TreeDictTest = functor (TD : DictionarySet.Set with
type Elt.t = IntKey.t) -> struct

  module T =  DictionarySet.Make(IntKey)(TreeDictionary.Make)
  type t = T.t

  let rec insert_all l d =
    match l with
    | [] -> d
    | h::t -> let d' = T.insert h d in 
              insert_all t d'

  let rec test4 l d =
    match l with
    | [] -> ()
    | h::t -> ignore(T.member h d);
              ignore(T.member h d);
              ignore(T.member h d);
              ignore(T.member h d);
              ignore(test4 t d)

  let time_tests l d =
    let timed_run = fun () -> d |> insert_all l |> test4 l in
    time timed_run

  let run_data =
      Printf.printf "n,List-Asc,List-Rnd\n";
      Printf.printf "100000,%f,%f\n" (time_tests sortedlist100k T.empty)
      (time_tests randomlist100k T.empty);
      Printf.printf "200000,%f,%f\n" (time_tests sortedlist200k T.empty)
      (time_tests randomlist200k T.empty);
      Printf.printf "300000,%f,%f\n" (time_tests sortedlist300k T.empty)
      (time_tests randomlist300k T.empty);
      Printf.printf "400000,%f,%f\n" (time_tests sortedlist400k T.empty)
      (time_tests randomlist400k T.empty);
      Printf.printf "500000,%f,%f\n" (time_tests sortedlist500k T.empty)
      (time_tests randomlist500k T.empty);
      Printf.printf "600000,%f,%f\n" (time_tests sortedlist600k T.empty)
      (time_tests randomlist600k T.empty);
      Printf.printf "700000,%f,%f\n" (time_tests sortedlist700k T.empty)
      (time_tests randomlist700k T.empty);
      Printf.printf "800000,%f,%f\n" (time_tests sortedlist800k T.empty)
      (time_tests randomlist800k T.empty);
      Printf.printf "900000,%f,%f\n" (time_tests sortedlist900k T.empty)
      (time_tests randomlist900k T.empty);
      Printf.printf "1000000,%f,%f\n" (time_tests sortedlist1M T.empty)
      (time_tests randomlist1M T.empty);

end
