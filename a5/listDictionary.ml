open Dictionary

(** [format_assoc_list fmt_key fmt_val fmt lst] formats an association 
    list [lst] as a dictionary.  The [fmt_key] and [fmt_val] arguments
    are formatters for the key and value types, respectively.  The
    [fmt] argument is where to put the formatted output. *)
(*BISECT-IGNORE-BEGIN*)
let format_assoc_list format_key format_val fmt lst =
  Format.fprintf fmt "[";
  List.iter (fun (k,v) -> Format.fprintf fmt "%a -> %a;"
                format_key k format_val v) lst;
  Format.fprintf fmt "]"
(*BISECT-IGNORE-END*)

module Make : DictionaryMaker
  = functor (K : KeySig) -> functor (V : ValueSig) ->
  struct
    module Key = K
    module Value = V
    type key = K.t
    type value = V.t

    (** Abstraction function: the List Dictionary [a1 -> b1; ...; an -> bn] 
        represents the dictionary with keys of set {a1, ..., an} and values of set 
        {b1, ..., bn}. [] represents the empty List Dictionary.
        Representation invariant: the List Dictionary contains no duplicates. *)

    type t = (key * value) list

    (*BISECT-IGNORE-BEGIN*)
    let compare x y =
      match Key.compare (fst x) (fst y) with
      | LT -> -1
      | EQ -> 0
      | GT -> 1

    let rep_ok d =
      d

    let empty = 
      []
    (*BISECT-IGNORE-END*)

    let is_empty d =
      match d with
      | [] -> true
      | h::t -> false

    let size d =
      List.length d

    let insert k v d =
      let exists = List.mem_assoc k d in

      if exists then let trim_dict = List.remove_assoc k d in 
        (k,v) :: trim_dict 
      else (k,v) :: d

    let remove k d =
      let exists = List.mem_assoc k d in

      if exists then List.remove_assoc k d 
      else d

    let find k d =
      let exists = List.mem_assoc k d in

      if exists then Some (List.assoc k d)
      else None

    let member k d =
      List.mem_assoc k d

    let rec keyList d acc = 
      match d with
      | [] -> acc
      | (h:'a*'b)::t -> (fst h)::acc

    let choose (d:t) =
      let l = size d in 
      let n = if l = 0 then 0 else Random.int l in
      if n = 0 && l = 0 then None else
        match d with
        | [] -> None
        | h::t -> Some (List.nth d n)

    (*BISECT-IGNORE-BEGIN*)
    let to_list d =
      List.sort compare d
    (*BISECT-IGNORE-END*)

    let fold f init d =
      to_list d |>
      List.fold_left (fun acc (k,v) -> f k v acc) init

    (*BISECT-IGNORE-BEGIN*)
    let format fmt d =
      format_assoc_list Key.format Value.format fmt d
      (*BISECT-IGNORE-END*)
  end
