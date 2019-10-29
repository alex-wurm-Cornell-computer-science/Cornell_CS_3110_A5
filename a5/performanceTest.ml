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

module L = DictionarySet.Make(IntKey)(ListDictionary.Make)
module T = DictionarySet.Make(IntKey)(TreeDictionary.Make)

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

(* module ListDictTest (L : Dictionary.Dictionary with 
type Key.t = int and type Value.t = int) (T : TreeDictionary ) *)

(* let istdicttest lst dict =

  let rec insert_all l d =
    match l with
    | [] -> d
    | [h::t] -> insert_all t (d.insert h 0 d) *)