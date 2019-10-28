open Dictionary

(** [format_elt_list fmt_key fmt lst] formats an element 
    list [lst] as a dictionary. The [fmt_key] argument
    is a formatter for the key type. The
    [fmt] argument is where to put the formatted output. *)
(*BISECT-IGNORE-BEGIN*)
let format_elt_list format_elt fmt lst =
  Format.fprintf fmt "[";
  List.iter (fun (k,v) -> Format.fprintf fmt "%a; "
                format_elt k) lst;
  Format.fprintf fmt "]"
(*BISECT-IGNORE-END*)

module type ElementSig = sig
  type t
  include Dictionary.KeySig with type t := t
end

module Unit = struct
  type t = unit
  let format fmt d =
    Format.fprintf fmt "()"
end

module type Set = sig
  module Elt : ElementSig
  type elt = Elt.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val choose : t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list
  val format : Format.formatter -> t -> unit
end

module Make =
  functor (E : ElementSig) ->
  functor (DM : DictionaryMaker) ->
  struct
    module Elt = E
    module Un = Unit
    type elt = Elt.t
    type un = Un.t 

    (** Abstraction function: the Dictionary Set [a1; ...; an] represents
        the dictionary with keys of set {a1, ..., an} and values of ().  
        [] represents the empty Dictionary Set.
        Representation invariant: the Dictionary Set contains no duplicates. *)

    module Dict = DM(Elt)(Un)

    type t = Dict.t

    (*BISECT-IGNORE-BEGIN*)
    let compare x y =
      match Elt.compare (fst x) (fst y) with
      | LT -> -1
      | EQ -> 0
      | GT -> 1

    let rep_ok s =
      s

    let empty =
      Dict.empty
    (*BISECT-IGNORE-END*)

    let is_empty s =
      Dict.is_empty s

    let size s =
      Dict.size s

    let insert x s =
      Dict.insert x () s

    let member x s =
      Dict.member x s 

    let remove x s =
      Dict.remove x s

    let choose s =
      let tup = Dict.choose s in 
      match tup with
      | None -> None
      | Some (a,b) -> Some a

    let fold f init s =
      Dict.fold (fun k v acc -> f k acc) init s

    let union s1 s2 =
      let rec unite_sets d1 d2 = 

        let l2 = Dict.to_list d2 in 

        match l2 with
        | [] -> d1
        | h::t -> let new_d1 = Dict.insert (fst h) (snd h) d1 in
          let new_d2 = Dict.remove (fst h) d2 in 
          unite_sets new_d1 new_d2
      in 

      unite_sets s1 s2

    let intersect s1 s2 =
      let rec intersect_sets d1 d2 acc =

        let l1 = Dict.to_list d1 in 
        match l1 with
        | [] -> d2
        | h::t -> if Dict.member (fst h) d2 then 
            intersect_sets (Dict.remove (fst h) d1) d2 
              (Dict.insert (fst h) (snd h) acc) 
          else intersect_sets (Dict.remove (fst h) d1) d2 acc
      in 

      intersect_sets s1 s2 Dict.empty

    let difference s1 s2 =
      let rec difference_sets d1 d2 acc =

        let l1 = Dict.to_list d1 in 
        match l1 with
        | [] -> acc
        | h::t -> if not (Dict.member (fst h) d2) then 
            difference_sets (Dict.remove (fst h) d1) d2 
              (Dict.insert (fst h) (snd h) acc) 
          else difference_sets (Dict.remove (fst h) d1) d2 acc
      in 

      difference_sets s1 s2 Dict.empty

    (*BISECT-IGNORE-BEGIN*)
    let to_list s =
      Dict.to_list s |> List.map fst

    let format fmt d =
      d |> Dict.to_list |> format_elt_list Elt.format fmt
      (*BISECT-IGNORE-END*)
  end
