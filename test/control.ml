
[%%rml.Control

let ratio = 1000000

let%signal s = {default = 0; gather = fun x y -> x + y}
let%signal s2 = {default = 1; gather = fun x y -> x * y}
let%signal s3 = {default = (0, 0); gather = fun x _ -> x}

let global_value = ref 0;;

let process spam n = 
  begin
  for i = 1 to n do
    emit s i;
    pause;
  done;
  emit s2 0;
  pause;
end

let process catch () =
  begin
    let a = ref true in
    try%control
      while !a do
        print_int !global_value;
        print_newline ();
        global_value := !global_value + 1;
        if%present s2 then a := false;
      done
    with [%event s] -> ()
  end

let process compare n =
  run (spam n) || run (catch ())
]

let compute_n n = begin
  Control.global_value := 0;
  Control.run (Control.compare n);
  !Control.global_value
end

let test_await () =
Alcotest.(check int) "control 1" (6) (compute_n 10);
Alcotest.(check int) "control 2" (10) (compute_n 18)

let test_set = [
("Control", `Quick, test_await)
]