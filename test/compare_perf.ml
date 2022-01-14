
let long_op n =
  let s = ref 0 in
  while (!s + !s - !s - n) <> 0 do
    incr s;
    incr s;
    decr s;
  done;;

let p1 t n =
  for i = 1 to t do
    long_op n
  done;;

[%%rml.Perf

  let process p2 t n =
    for%par i = 1 to t do
      [%ocaml long_op] n
    done

  let process p3 t n =
    for i = 1 to t do
      [%ocaml long_op] n
    done
]

let time nbProcess n =
  let t1 = Unix.gettimeofday () in
  let () = Perf.run (Perf.p2 nbProcess n) in
  let t2 = Unix.gettimeofday () in
  let () = p1 nbProcess n in
  let t3 = Unix.gettimeofday () in
  let () = Perf.run (Perf.p3 nbProcess n) in
  let t4 = Unix.gettimeofday () in
  Printf.printf "Execution time for rml par: %fs\n" (t2 -. t1);
  Printf.printf "Execution time for rml seq: %fs\n" (t4 -. t3);
  Printf.printf "Execution time for ocaml  : %fs\n" (t3 -. t2)

let () = time 3 200000000