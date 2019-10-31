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

module TreeDictTest = TreeDictionary.Make(IntKey)(IntKey)

module T = DictionarySet.Make(IntKey)(TreeDictionary.Make)

let rec insert_list lst acc =
  match lst with
  | [] -> acc
  | h::t -> insert_list t (T.insert h acc)

let rec list_mem_test_tree lst d =
  match lst with
  | [] -> false
  | h::t -> (T.member h d) && list_mem_test_tree t d

let rec four_times lst d i = 
  if i = 0 then true
  else list_mem_test_tree lst d && four_times lst d (i-1)

let test_insert_tree lst i = 
  let d = insert_list lst T.empty in
  four_times lst d i

module L = DictionarySet.Make(IntKey)(ListDictionary.Make)

let rec insert_list_list lst acc =
  match lst with
  | [] -> acc
  | h::t -> insert_list_list t (L.insert h acc)

let rec list_mem_test_list lst d =
  match lst with
  | [] -> false
  | h::t -> (L.member h d) && list_mem_test_list t d

let rec four_times_list lst d i = 
  if i = 0 then true
  else list_mem_test_list lst d && four_times_list lst d (i-1)

let test_insert_list lst i = 
  let d = insert_list_list lst L.empty in
  four_times_list lst d i



(* random list *)
let _ = print_endline ("LIST DICTIONARY - RANDOM LISTS")
let _ = time (fun()->(ignore(test_insert_list (rnd_list 1000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_list 2000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_list 3000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_list 4000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_list 5000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_list 6000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_list 7000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_list 8000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_list 9000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_list 10000) 4))) |> string_of_float |> print_endline
let _ = print_endline ("END OF LIST DICTIONARY - RANDOM LISTS")

(* sorted list *)
let _ = print_endline ("LIST DICTIONARY - SORTED LISTS")
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 1000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 2000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 3000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 4000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 5000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 6000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 7000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 8000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 9000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_list (rnd_sort_list 10000) 4))) |> string_of_float |> print_endline
let _ = print_endline ("END OF LIST DICTIONARY - SORTED LISTS")





(* random tree list *)
let _ = print_endline ("TREE DICTIONARY - RANDOM LISTS")
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 100000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 200000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 300000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 400000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 500000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 600000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 700000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 800000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 900000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_list 1000000) 4))) |> string_of_float |> print_endline
let _ = print_endline ("END OF TREE DICTIONARY - RANDOM LISTS")

(* sorted tree list *)
let _ = print_endline ("TREE DICTIONARY - SORTED LISTS")
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 100000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 200000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 300000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 400000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 500000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 600000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 700000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 800000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 900000) 4))) |> string_of_float |> print_endline
let _ = time (fun()->(ignore(test_insert_tree (rnd_sort_list 1000000) 4))) |> string_of_float |> print_endline
let _ = print_endline ("END OF TREE DICTIONARY - SORTED LISTS")
