open Dictionary

(** [format_tree_list fmt_key fmt_val fmt lst] formats an element 
    list [lst] as a treedictionary. The [fmt_key] argument
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

    (* AF:  Tree Dictionary [(c1,v1,l1,r1); ... ; (cn,vn,ln,rn)] represents a 
       tree of interconnected nodes, b1...bn. Each node has a color, a value, a left
       child node, and a right child node. A tree can be visualized as such:
                                     b1
                                    /   \
                                  b2    b3
                                 / \    / \
                                b4 b5 b6  b7
                                    . . .
       The empty Tree Dictionary is a Leaf (i.e. a node with no children nodes and 
       no value).

     * RI: 
       - No Red node may have a Red child. 
       - All branches of the tree must have the same number of Black nodes. 
       - No branch of the tree can have a depth of at least two times 
          greater than that of any other branch. 
       - The left child of any node must have a key that is less than that of
          its parent node. 
       - The right child of any node must have a key that is greater than 
          that of its parent node. *)

    (* [Exception NotOkay] is thrown if the representation invariant is
       violated; for use when checking rep_ok. *)
    exception NotOkay

    type color = Red | Black 

    type t = 
      | Leaf 
      | Node of color * (key*value) * t * t

    (* [fold_tree f acc d] is the result of recursively applying function [f]
       to a [TreeDictionary] by traversing the tree from root to leaves. *)
    let rec fold_tree f acc d = 
      match d with
      | Leaf -> acc
      | Node (_,v,l,r)  -> f v (fold_tree f acc l) (fold_tree f acc r)

    (* [max_depth d] is the depth of the longest branch of 
       [TreeDictionary d].*)
    let max_depth d =
      fold_tree (fun _ l r -> 1 + max l r) 0 d

    (* [min_depth d] is the depth of the shortest branch of 
       [TreeDictionary d].*)
    let min_depth d =
      fold_tree (fun _ l r -> 1 + min l r) 0 d

    (* [is_bst d] is the boolean value of the statement '[d] is a valid 
       Binary Search Tree'. The function checks that for each node in the 
       [TreeDictionary d], the value of the left child is less than the value
       of the node, and the value of the right child is greater than the value
       of the node. *)
    let rec is_bst d =
      match d with 
      | Leaf -> false
      | Node (_,v,l,r) ->  match (l,r) with
        | (Leaf,Leaf) -> true
        | (Leaf, Node (_,x,_,_)) -> is_bst r && x > v
        | (Node (_,y,_,_)), Leaf -> is_bst l && y < v
        | (Node (_,a,_,_), Node (_,b,_,_)) -> 
          is_bst l && is_bst r && a < v && b > v

    (* [no_double_reds d] is the boolean value of the statement 'd satisfies
       the first representation invariant for red-black trees that no red node 
       has a red child.' The function recursively traverses the tree from the 
       root to the leaves and immediately returns false if it encounters any
       red node with a red child, or else returns true if it does not encounter
       any such nodes. *)
    let rec no_double_reds d =
      match d with
      | Leaf -> false
      | Node (Red,_,l,r) -> (match (l,r) with 
          | (Leaf,Leaf) -> true
          | (Leaf, Node (Red,_,_,_)) -> false
          | (Node (Red,_,_,_), Leaf) -> false
          | (Node (Red,_,_,_), Node (Red,_,_,_)) -> false
          | (Node (Red,_,_,_), Node (Black,_,_,_)) -> false
          | (Node (Black,_,_,_), Node (Red,_,_,_)) -> false
          | (Node (Black,_,_,_), Leaf) -> no_double_reds l
          | (Leaf, Node (Black,_,_,_)) -> no_double_reds r
          | Node (Black,_,_,_), Node (Black,_,_,_) ->
            no_double_reds l && no_double_reds r
        )
      | Node (Black,_,l,r) -> (match (l,r) with
          | (Leaf,Leaf) -> true
          | (Node (_,_,_,_), Leaf) -> no_double_reds l
          | (Leaf, Node (_,_,_,_)) -> no_double_reds r
          | (Node (_,_,_,_), Node (_,_,_,_)) -> 
            no_double_reds l && no_double_reds r
        )

    (* [same_black_lengths d] is the boolean value of the statement 'd satisfies
       the second representation invariant for red-black trees that all branches in
       the tree must have the same number of black nodes.' The function recursively
       traverses the tree from the root to the leaves and checks that both children 
       of each node have the same length, immediately raising the exception
       NotOkay if any two sub-branches are encounter with different numbers
       of black nodes. *)
    let rec same_black_lengths d = 
      match d with
      | Leaf -> 0
      | Node (c,_,l,r) -> (match (same_black_lengths l, same_black_lengths r) with
          | (x,y) -> if x = y && c = Black then x + 1
            else if x = y && c = Red then x
            else raise NotOkay
        )


    (* [rep_ok d] is the [TreeDictionary] d if d satisfies the 4 representation
       invariants of a red-black balanced BST which are:
       1) the tree is balanced meaning max_depth <= 2 * min_depth
       2) the BST invariant is maintained (the left child of a node always
       has a value less than the node and the right child of a node always has
       a value greater than the node.
       3) red nodes do not have red children
       4) all branches have the same number of black nodes
       If any of the representation invariants are not satisfied, [rep_ok d]
       is a failure message identifying which invariant was violated. *)

    let rep_ok d =
      if d = Leaf then d else (
        if (max_depth d) > 2 * (min_depth d) 
        then failwith "tree is not balanced"
        else if not (is_bst d)
        then failwith "not a BST" 
        else if not (no_double_reds d) 
        then failwith "RB invariant #1 violated"
        else match same_black_lengths d with
          | x -> d
          | exception NotOkay -> failwith "RB invariant #2 violated"
      )

    let empty = 
      Leaf 

    let is_empty d =
      d = Leaf

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

    let rec ins k v d = 
      match d with 
      | Leaf -> Node (Red, (k,v), Leaf, Leaf)
      | Node (c,w,l,r) as d -> 
        let comparison = Key.compare k (fst w) in
        if comparison = EQ then d 
        else if comparison = LT then balance_tree (c, w, ins k v l, r)
        else balance_tree (c, w, l, ins k v r)

    let insert k v d =
      match ins k v d with 
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
