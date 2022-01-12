
[%%rml.When

let ratio = 1000000

let s = Signal {default = 0; gather = fun x y -> x + y}
let s2 = Signal {default = 1; gather = fun x y -> x * y}
let s3 = Signal {default = (0, 0); gather = fun x _ -> x}

let global_value = ref 0;;

let process spam n = 
  begin
  for i = 1 to n do
    pause;
    emit s i;
    
  done;
  emit s2 0;
  pause;
end

let process catch () =
  begin
    let a = ref true in
    try%when
      while !a do
        print_int !global_value;
        print_newline ();
        global_value := !global_value + 1;
        if%present s2 then a := false;

      done
    with [%event s] -> () (* compilation normally fails when adding `when (0 = 0)` *)
  end

let process compare n =
  run (spam n) || run (catch ())
]

let compute_n n = begin
  When.global_value := 0;
  When.run (When.compare n);
  !When.global_value
end

let test_await () =
Alcotest.(check int) "do..when.. 1" (10) (compute_n 10);
Alcotest.(check int) "do..when.. 2" (100) (compute_n 100)


let test_set = [
("Do When", `Quick, test_await)
]