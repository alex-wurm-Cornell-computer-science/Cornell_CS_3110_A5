module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val words : idx -> string list
  val to_list : idx -> (string * string list) list
  val or_not  : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end

module Make = 
  functor (S:DictionarySet.Set with type Elt.t = string)
    -> functor (D:Dictionary.Dictionary with type Key.t = string
                                         and type Value.t = S.t) 
    -> struct
      (* TODO: replace [unit] with a type of your own design. *)
      (** AF: TODO: document the abstraction function.
          RI: TODO: document any representation invariants. *)
      type idx = unit

      let index_of_dir d =
        failwith "Unimplemented"

      let words idx = 
        failwith "Unimplemented"

      let to_list idx =
        failwith "Unimplemented"

      let or_not idx ors nots =
        failwith "Unimplemented"

      let and_not idx ands nots =
        failwith "Unimplemented"

      let format fmt idx =
        Format.fprintf fmt "<unimplemented>" 
    end