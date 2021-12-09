module type S = sig end

[%%rml
(* Definition of binary trees. *)
type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

(* Breadth first traveral. *)
let rec process iter_breadth f t =
  match t with
    | Empty -> ()
    | Node (x, l, r) ->
      f x;
      pause;
      run (iter_breadth f l) || run (iter_breadth f r)
]