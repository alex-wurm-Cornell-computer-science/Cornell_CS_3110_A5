open OUnit2
open ListDictionary
open Dictionary

module Int = struct
  type t = int
  let compare x y =
    match Stdlib.compare x y with
    | x when x<0 -> LT
    | 0 -> EQ
    | _ -> GT
  let format fmt x =
    Format.fprintf fmt "%d" x
end;;

(* The next line creates a dictionary that maps ints to ints. *)
module IntIntDictionary = ListDictionary.Make(Int)(Int)

let make_is_empty
    (name : string)
    (input : IntIntDictionary.t)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.is_empty input))

let make_size
    (name : string)
    (dict : IntIntDictionary.t)
    (expected_output : int): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.size dict))

let make_insert
    (name : string)
    (key : int)
    (value : int)
    (dict : IntIntDictionary.t)
    (expected_output : IntIntDictionary.t): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.insert key value dict))

let make_remove
    (name : string)
    (key : int)
    (dict : IntIntDictionary.t)
    (expected_output : IntIntDictionary.t): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.remove key dict))

let make_find
    (name : string)
    (key : int)
    (dict : IntIntDictionary.t)
    (expected_output : int option): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (IntIntDictionary.find key dict))

let make_member
    (name : string)
    (key : int)
    (dict : IntIntDictionary.t)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.member key dict))

let make_choose
    (name : string)
    (dict : IntIntDictionary.t)
    (expected_output : (int * int) option): test = 
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.choose dict))

let make_to_list
    (name : string)
    (dict : IntIntDictionary.t)
    (expected_output : (int * int) list): test = 
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.to_list dict))

let make_fold
    (name : string)
    (func : IntIntDictionary.key -> IntIntDictionary.value -> 'a -> 'a)
    (init : int)
    (dict : IntIntDictionary.t)
    (expected_output : int): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.fold func init dict))

let d_empty = IntIntDictionary.empty
let d_add_1 = IntIntDictionary.insert 1 1 (IntIntDictionary.empty)
let d_choose_1 = (1,1)
let d_remove_1 = IntIntDictionary.remove 1 d_add_1
let d_add_2a = IntIntDictionary.insert 6 2 d_remove_1
let d_choose_2a = (6,2)
let d_add_2b = IntIntDictionary.insert 12 3 d_add_2a
let d_add_existing = IntIntDictionary.insert 6 7 d_add_2b

let list_dictionary_tests = [
  make_is_empty "is_empty: created empty listdict" d_empty true;
  make_size "size: created empty listdict" d_empty 0;
  make_choose "choose: created empty listdict" d_empty None;

  make_insert "insert: added one key*val pair" 1 1 d_empty d_add_1;
  make_is_empty "is_empty: added one key*val pair" d_add_1 false;
  make_size "size: added one key*val pair" d_add_1 1;
  make_choose "choose: from dict with one element" d_add_1 (Some d_choose_1);
  make_fold "fold: sum of dict with one element" (fun k v acc -> k*2 + v*3 + acc) 0 d_add_1 5;

  make_remove "remove: removed one key*val pair" 1 d_add_1 d_empty;
  make_is_empty "is_empty: removed one key*val pair" d_remove_1 true;
  make_size "size: removed one key*val pair" d_remove_1 0;
  make_choose "choose: from newly empty listdict" d_remove_1 None;
  make_fold "fold: sum of dict with empty listdict" (fun k v acc -> k*2 + v*3 + acc) 0 d_remove_1 0;

  make_insert "insert: add one of two key*val pairs" 6 2 d_remove_1 d_add_2a;
  make_is_empty "is_empty: add one of two key*val pairs" d_add_2a false;
  make_size "size: add one of two key*val pairs" d_add_2a 1;
  make_choose "choose: add one of two key*val pairs" d_add_2a (Some d_choose_2a);
  make_fold "fold: sum of dict with one element" (fun k v acc -> k*2 + v*3 + acc) 0 d_add_2a 18;

  make_insert "insert: add two of two key*val pairs" 12 3 d_add_2a d_add_2b;
  make_is_empty "is_empty: add two of two key*val pairs" d_add_2b false;
  make_size "size: add two of two key*val pairs" d_add_2b 2;
  make_find "find: add two of two key*val pairs" 12 d_add_2b (Some 3);
  make_member "member: add two of two key*val pairds" 12 d_add_2b true;
  make_find "find: looking for nonexisting element" 10 d_add_2b None;
  make_member "member: looking for nonexisting element" 10 d_add_2b false;
  make_fold "fold: sum of dict with one element" (fun k v acc -> k*2 + v*3 + acc) 0 d_add_2b 51;
  make_insert "insert: adding element with existing key" 6 7 d_add_2b d_add_existing;
  make_is_empty "is_empty: add two of two key*val pairs" d_add_existing false;
  make_size "size: add two of two key*val pairs" d_add_existing 2;
  make_fold "fold: sum of dict with one element" (fun k v acc -> k*2 + v*3 + acc) 0 d_add_existing 66;
]

module DictMake = ListDictionary.Make(Int)(Int)
module IntDictionarySet = DictionarySet.Make(Int)(ListDictionary.Make);;

let make_dictset_is_empty
    (name : string)
    (input : IntDictionarySet.t)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.is_empty input))

let make_dictset_size
    (name : string)
    (dict : IntDictionarySet.t)
    (expected_output : int): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.size dict))

let make_dictset_insert
    (name : string)
    (key : int)
    (dict : IntDictionarySet.t)
    (expected_output : IntDictionarySet.t): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.insert key dict))

let make_dictset_member
    (name : string)
    (key : int)
    (dict : IntDictionarySet.t)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.member key dict))

let make_dictset_remove
    (name : string)
    (key : int)
    (dict : IntDictionarySet.t)
    (expected_output : IntDictionarySet.t): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.remove key dict))

let make_dictset_union
    (name : string)
    (d1 : IntDictionarySet.t)
    (d2 : IntDictionarySet.t)
    (expected_output : IntDictionarySet.t): test = 
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.union d1 d2))

let make_dictset_intersect
    (name : string)
    (d1 : IntDictionarySet.t)
    (d2 : IntDictionarySet.t)
    (expected_output : IntDictionarySet.t): test = 
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.intersect d1 d2))

let make_dictset_difference
    (name : string)
    (d1 : IntDictionarySet.t)
    (d2 : IntDictionarySet.t)
    (expected_output : IntDictionarySet.t): test = 
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.difference d1 d2))

let make_dictset_choose
    (name : string)
    (dict : IntDictionarySet.t)
    (expected_output : int option): test = 
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.choose dict))

let make_dictset_fold
    (name : string)
    (func : (IntDictionarySet.elt -> 'acc -> 'acc))
    (init : int)
    (dict : IntDictionarySet.t)
    (expected_output : int): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntDictionarySet.fold func init dict))

let dset_empty = IntDictionarySet.empty
let dset_add_1 = IntDictionarySet.insert 1 (IntDictionarySet.empty)
let dset_remove_1 = IntDictionarySet.remove 1 dset_add_1
let dset_add_2a = IntDictionarySet.insert 6 dset_remove_1
let dset_add_2b = IntDictionarySet.insert 12 dset_add_2a
let dset_add_just12 = IntDictionarySet.insert 12 dset_empty
let dset_add_existing = IntDictionarySet.insert 6  dset_add_2b

let dictionary_set_tests = [
  make_dictset_is_empty "is_empty: created empty listdict" dset_empty true;
  make_dictset_size "size: created empty listdict" dset_empty 0;
  make_dictset_choose "choose: created empty listdict" dset_empty None;
  make_dictset_union "union: empty set with itself" dset_empty dset_empty dset_empty;
  make_dictset_intersect "intersect: empty set with itself" dset_empty dset_empty dset_empty;
  make_dictset_difference "difference: empty set with itself" dset_empty dset_empty dset_empty;

  make_dictset_insert "insert: added one key*val pair" 1 dset_empty dset_add_1;
  make_dictset_is_empty "is_empty: added one key*val pair" dset_add_1 false;
  make_dictset_size "size: added one key*val pair" dset_add_1 1;
  make_dictset_choose "choose: from dict with one element" dset_add_1 (Some 1);
  make_dictset_fold "fold: sum of dict with one element" (fun k acc -> k*2 + acc) 0 dset_add_1 2;
  make_dictset_union "union: empty set with set of one element" dset_add_1 dset_empty dset_add_1;
  make_dictset_intersect "intersect: empty set with set of one element" dset_add_1 dset_empty dset_empty;
  make_dictset_difference "difference: empty set with set of one element" dset_add_1 dset_empty dset_add_1;

  make_dictset_remove "remove: removed one key*val pair" 1 dset_add_1 dset_empty;
  make_dictset_is_empty "is_empty: removed one key*val pair" dset_remove_1 true;
  make_dictset_size "size: removed one key*val pair" dset_remove_1 0;
  make_dictset_choose "choose: from newly empty listdict" dset_remove_1 None;
  make_dictset_fold "fold: sum of dict with empty listdict" (fun k acc -> k*2 + acc) 0 dset_remove_1 0;

  make_dictset_insert "insert: add one of two key*val pairs" 6 dset_remove_1 dset_add_2a;
  make_dictset_is_empty "is_empty: add one of two key*val pairs" dset_add_2a false;
  make_dictset_size "size: add one of two key*val pairs" dset_add_2a 1;
  make_dictset_choose "choose: add one of two key*val pairs" dset_add_2a (Some 6);
  make_dictset_fold "fold: sum of dict with one element" (fun k acc -> k*2 + acc) 0 dset_add_2a 12;

  make_dictset_insert "insert: add two of two key*val pairs" 12 dset_add_2a dset_add_2b;
  make_dictset_is_empty "is_empty: add two of two key*val pairs" dset_add_2b false;
  make_dictset_size "size: add two of two key*val pairs" dset_add_2b 2;
  make_dictset_member "member: add two of two key*val pairds" 12 dset_add_2b true;
  make_dictset_member "member: looking for nonexisting element" 10 dset_add_2b false;
  make_dictset_fold "fold: sum of dict with one element" (fun k acc -> k*2 + acc) 0 dset_add_2b 36;
  make_dictset_union "union: two sets" dset_add_2a dset_add_2b dset_add_2b;
  make_dictset_intersect "intersect: two sets" dset_add_2b dset_add_2a dset_add_2a;
  make_dictset_difference "difference: two sets" dset_add_2b dset_add_2a dset_add_just12;
]

module String = struct
  type t = string
  let compare x y =
    match Stdlib.compare x y with
    | x when x<0 -> LT
    | 0 -> EQ
    | _ -> GT
  let format fmt x =
    Format.fprintf fmt "%s" x
end;;

module S = DictionarySet.Make(String)(ListDictionary.Make)
module D = ListDictionary.Make(String)(DictionarySet.Make(String)(ListDictionary.Make))
module E = Engine.Make(S)(D)



let make_engine_words
    (name : string)
    (idx : E.idx)
    (expected_output : int): test = 
  name >:: (fun _ ->
      assert_equal expected_output (List.length (E.words idx)))

let make_or_not
    (name : string)
    (idx : E.idx)
    (ors : string list)
    (nots : string list)
    (expected_output : string list): test =
  name >:: (fun _ ->
      assert_equal expected_output (E.or_not idx ors nots))

let make_and_not
    (name : string)
    (idx : E.idx)
    (ands : string list)
    (nots : string list)
    (expected_output : string list): test =
  name >:: (fun _ ->
      assert_equal expected_output (E.and_not idx ands nots))

let alice_idx = E.index_of_dir "alice"
let alice_words = E.words alice_idx
let alice_ors = ["directed"; "taxes"; "as"]
let alice_nots = ["64-6221541"; "provoking"]
let alice_nots_2 = ["bla"]
let preamble_idx = E.index_of_dir "preamble"
let preamble_words = E.words preamble_idx
let preamble_ors = ["we";"the";"people"] 
let preamble_nots = ["america"]
let preamble_nots_2 = ["heehee"]

let engine_tests = [
  make_engine_words "alice directory" alice_idx 3278;
  make_engine_words "preamble directory" preamble_idx 38;

  make_or_not "or not: testing on alice" alice_idx alice_ors alice_nots [];
  make_or_not "or not 2: testing on alice" alice_idx alice_ors alice_nots_2 ["alice.txt"];
  make_or_not "or not: testing on preamble" preamble_idx preamble_ors preamble_nots ["prefix.txt"];
  make_or_not "or not 2: testing on preamble" preamble_idx preamble_ors preamble_nots_2 ["prefix.txt";"whole.txt"];

  make_and_not "and not: testing on alice" alice_idx alice_ors alice_nots [];
  make_and_not "and not 2: testing on alice" alice_idx alice_ors alice_nots_2 ["alice.txt"];
  make_and_not "and not: testing on preamble" preamble_idx preamble_ors preamble_nots ["prefix.txt"];
  make_and_not "and not 2: testing on preamble" preamble_idx preamble_ors preamble_nots_2 ["prefix.txt";"whole.txt"];
]

let suite = "search test suite" >::: List.flatten [ 
    list_dictionary_tests;
    dictionary_set_tests;
    engine_tests;
  ]

let _ = run_test_tt_main suite
