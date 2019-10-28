(** Dictionaries implemented as association lists. *)

open Dictionary

(** [Make] makes a [Dictionary] implemented
    with association lists.  *)
module Make : DictionaryMaker

val format_assoc_list : 
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a * 'b) list -> unit