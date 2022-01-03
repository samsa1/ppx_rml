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

let rec build_random n =
  if n = 0
  then Empty
  else
    let l = Random.int (n + 1) in
    let r = Random.int (n - l + 1) in  
    Node (n - l - r, build_random l, build_random r)
]

let compute_n n = 
  let tree = Rml.build_random n in
  let nb = ref 0 in
  Rml.run (Rml.iter_breadth (fun n -> nb := !nb + n) tree);
  !nb

let test_rififi n () =
  Alcotest.(check int) "1" (n * 1) (compute_n (n * 1));
  Alcotest.(check int) "2" (n * 2) (compute_n (n * 2));
  Alcotest.(check int) "3" (n * 3) (compute_n (n * 3));
  Alcotest.(check int) "4" (n * 4) (compute_n (n * 4))

let test_set = [
  ("Small", `Quick, test_rififi 10);
  ("Medium", `Quick, test_rififi 100)
]