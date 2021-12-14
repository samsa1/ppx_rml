
[%%rml.Signals

  let ratio = 1000000

  let%signal s = {default = 0; gather = fun x y -> x + y}
  let%signal s2 = {default = 1; gather = fun x y -> x * y}

  let process spam n = 
    for%par i = 1 to n do
      emit s i;
      emit s2 i;
      pause;
      emit s (i * 2);
    done

  let process catch p =
    let%await _ = i1 = s && i2 = s2 in
    let%await _ = i3 = s in
    p := (i1 * ratio * ratio + i2 * ratio + i3)


  let process compare n =
    let p = ref 0 in run (spam n) || run (catch p); !p
]

let compute_n_seq n =
  let rec fac n = if n < 2 then 1 else n * fac (n - 1) in 
  let n2 = ((n + 1) * n) / 2 in
  Signals.ratio * Signals.ratio * n2 + Signals.ratio * (fac n) + (2 * n2)

open Rmllib.Implem_lco_ctrl_tree_record

let compute_n n = 
  Rml_machine.rml_exec [] (fun () -> Lco_ctrl_tree_record.rml_run (fun () -> (Signals.compare n)))


let test_await () =
  Alcotest.(check int) "await 1" (compute_n_seq 10) (compute_n 10)

let test_set = [
  ("await", `Quick, test_await)
]