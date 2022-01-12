
[%%rml.Signals

  let ratio = 1000000

  let s = Signal {default = 0; gather = fun x y -> x + y}
  let s2 = Signal {default = 1; gather = fun x y -> x * y}
  let s3 = Signal {default = (0, 0); gather = fun x _ -> x}
  let s4 = Signal {default = []; gather = fun x y -> x :: y}
  let sp0 = Signal {default = 0; gather = fun x _ -> x}
  let sp1 = Signal {default = 0; gather = fun x _ -> x}
  let sp2 = Signal {default = 0; gather = fun x _ -> x}
  let sp3 = Signal {default = 0; gather = fun x _ -> x}

  let process a () =
    let sp4 = Signal {default = 0; gather = fun x _ -> x}
      and _useless_0 = 42
      and sp5 = Signal {default = 0; gather = fun x _ -> x}
      and _useless_1 = 69
      in ((emit sp4 _useless_0) || (emit sp5 _useless_1))

  let process spam n = 
    for%par i = 1 to n do
      emit s i;
      emit s2 i;
      pause;
      emit s (i * 2);
    done

  let process catch p =
    (* Here, both s3 and s4 won't ver be present (never emitted) but it shows the macro works :tm: *)
    let%await All = (i1 = s && i2 = s2) || (i1, i2) = s3 || [i1; i2] = s4 in
    let%await All = i3 = s in
    p := (i1 * ratio * ratio + i2 * ratio + i3)


  let process compare n =
    let p = ref 0 in run (spam n) || run (catch p) || (run (a ())); !p
  
  let process present_1 =
    let c = (if%present sp0 then 42 else 69) in
    emit sp1 0;
    let d = if%present sp1 then 69 else 42 in
    emit sp2 0;
    let e = if%present sp2 && sp3 then 42 else 69 in
    (* We need to re-emit it because the last line lasted an entire step as it went through the `else` *)
    emit sp2 0;
    let f = if%present sp2 || sp3 then 69 else 42 in
    [c; d; e; f]

]

let compute_n_seq n =
  let rec fac n = if n < 2 then 1 else n * fac (n - 1) in 
  let n2 = ((n + 1) * n) / 2 in
  Signals.ratio * Signals.ratio * n2 + Signals.ratio * (fac n) + (2 * n2)

let compute_n n = 
  Signals.run (Signals.compare n)

let compute_present () = 
  Signals.run (Signals.present_1)

let test_await () =
  Alcotest.(check int) "await 1" (compute_n_seq 10) (compute_n 10);
  Alcotest.(check (list int)) "present 1" (compute_present ()) [69; 69; 69; 69]

let test_set = [
  ("await", `Quick, test_await)
]