
[%%rml.Control
let s = Signal {default = 0; gather = fun x y -> x + y}
let s2 = Signal {default = 1; gather = fun x y -> x * y}

let global_value = ref 0;;

let process spam n = 
  begin
  for i = 1 to n do
    emit s 0;
    pause;
  done;
  emit s2 0;
  pause;
end

let process catch n =
  begin
    let a = ref true in
    try%control
      while !a do
        print_int !global_value;
        print_newline ();
        global_value := !global_value + 1;
        if%present s2 then a := false;
      done
    with [%event (a = s) || (a = s2)] when a = n -> () (* Can add a guard condition *)
  end

let process compare n m =
  run (spam n) || run (catch m)
]

let compute_n n m = begin
  Control.global_value := 0;
  Control.run (Control.compare n m);
  !Control.global_value
end

let test_await () =
  (* As `spam` emits 0s on `s`, the loop in catch gets switched all the time *)
Alcotest.(check int) "control 1" (6) (compute_n 10 0);
  (* This time, it doesn't get switched, therefore the higher resulting value *)
Alcotest.(check int) "control 2" (11) (compute_n 10 1);
Alcotest.(check int) "control 3" (10) (compute_n 18 0)

let test_set = [
("Control", `Quick, test_await)
]