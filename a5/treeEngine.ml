(* Note that there is nothing you need to complete in this file. *)

open Dictionary

(** [StringKey] provides the necessary definitions to use strings
    as keys in dictionaries. *)
module StringKey
  : KeySig with type t = string
=
struct
  type t = string
  let compare s1 s2 =
    match Stdlib.compare s1 s2 with
    | x when x < 0 -> LT
    | x when x > 0 -> GT
    | _ -> EQ
  let format fmt s =
    Format.fprintf fmt "\"%s\"" s
end

(** [S] is a dictionary set implemented with a [TreeDictionary]
    whose keys are strings. *)
module S = DictionarySet.Make(StringKey)(TreeDictionary.Make)

(** [D] is a [TreeDictionary] whose keys are strings. *)
module D = TreeDictionary.Make(StringKey)(S)


module TreeEngine = Engine.Make(S)(D)
