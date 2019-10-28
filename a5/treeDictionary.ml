open Dictionary

(** [format_elt_list fmt_key fmt lst] formats an element 
    list [lst] as a dictionary. The [fmt_key] argument
    is a formatter for the key type. The
    [fmt] argument is where to put the formatted output. *)
(*BISECT-IGNORE-BEGIN*)
let format_tree_list format_key format_val fmt lst =
  Format.fprintf fmt "[";
  List.iter (fun (k,v) -> Format.fprintf fmt "%a -> %a; "
                format_key k format_val v) lst;
  Format.fprintf fmt "]"
(*BISECT-IGNORE-END*)

module Make
  = functor (K : KeySig) -> functor (V : ValueSig) ->
  struct
    module Key = K
    module Value = V
    type key = K.t
    type value = V.t

    (* TODO: change type [t] from [unit] to something involving
       red-black trees. *)
    (* AF: TODO: document the abstraction function.
     * RI: TODO: document any representation invariants. *)
    type color = Red | Black 

    type t = 
      | Leaf 
      | Node of color * (key*value) * t * t

    let rep_ok d =
      failwith "Unimplemented"

    let empty = 
      Leaf (* TODO: replace [()] with a value of your rep type [t]. *)

    let is_empty d =
      d = Leaf

    let rec fold_tree f acc d = 
      match d with
      | Leaf -> acc
      | Node (_,v,l,r)  -> f v (fold_tree f acc l) (fold_tree f acc r)

    let size d =
      fold_tree (fun _ l r -> 1 + l + r) 0 d 

    let balance_tree tree =
      match tree with 
      | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
      | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
      | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
      | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | a, b, c, d -> Node (a, b, c, d)

    let insert k v d =
      let rec ins d = 
        match d with 
        | Leaf -> Node (Red, (k,v), Leaf, Leaf)
        | Node (c,w,l,r) as d -> 
          if (k < fst w) then balance_tree (c, w, ins l, r)
          else if (k > fst w) then balance_tree (c, w, l, ins r)
          else d in 
      match ins d with 
      | Node (_,w,l,r) -> Node (Black, w, l, r)
      | Leaf -> failwith "RBT insert failed with ins returning leaf"

    let remove k d =
      failwith "Unimplemented"

    let rec find k d =
      match d with 
      | Leaf -> None
      | Node (_,w,l,r) -> 
        if k = fst w then Some (snd w) 
        else if k < fst w then find k l 
        else if k > fst w then find k r
        else failwith "RBT find failed with find returning leaf"


    let rec member k d =
      match d with 
      | Leaf -> false
      | Node (_,w,l,r) -> 
        k = fst w || (k < fst w && member k l) || (k > fst w && member k r)

    let rec choose d =
      let n = Random.int 2 in
      match d with
      | Leaf -> None 
      | Node (_,w,l,r) -> 
        if n = 0 then Some w 
        else if n = 1 then choose l
        else if n = 2 then choose r
        else None

    let to_list d =
      let rec to_list_helper acc d = 
        match d with
        | Leaf -> acc
        | Node (_,w,l,r) -> (to_list_helper ((w :: (to_list_helper acc r))) l)
      in to_list_helper [] d

    let rec fold f acc d =
      match d with
      | Leaf -> acc
      | Node (_,(k,v),l,r)  -> f k v (fold f (fold f acc l) r)

    let format fmt d =
      d |> to_list |> format_tree_list Key.format Value.format fmt

  end
