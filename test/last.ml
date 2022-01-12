[%%rml.Last
  let test0_s = Signal {default = 69; gather = (fun x _ -> x)};;
  let test0_s_default = default test0_s;;
  let v_0_1 = last test0_s;;
  emit test0_s 42;;
  pause;;
  let v_0_3 = last test0_s;;

  let test1_s = Signal {default = []; gather = fun x y -> x :: y};;
  let test1_s_default = default test1_s;;
  let v_1_1 = last test1_s;;
  emit test1_s 42;;
  pause;;
  let v_1_3 = last test1_s;;

  let test2_s = Signal {default = [69]; gather = fun x y -> x :: y};;
  let test2_s_default = default test2_s;;
  let v_2_1 = last test2_s;;
  emit test2_s 42;;
  pause;;
  let v_2_3 = last test2_s;;

]

let test_last_0 () =
  Alcotest.(check int) "Last conf 0 initial" Last.v_0_1 69; 
  Alcotest.(check int) "Last conf 0 initial default" Last.v_0_1 Last.test0_s_default;
  Alcotest.(check int) "Last conf 0 final" Last.v_0_3 42
  
let test_last_1 () =
  Alcotest.(check (list int)) "Last conf 1 initial" Last.v_1_1 []; 
  Alcotest.(check (list int)) "Last conf 1 initial default" Last.v_1_1 Last.test1_s_default;
  Alcotest.(check (list int)) "Last conf 1 final" Last.v_1_3 [42]

let test_last_2 () =
  Alcotest.(check (list int)) "Last conf 2 initial" Last.v_2_1 [69]; 
  Alcotest.(check (list int)) "Last conf 2 initial default" Last.v_2_1 Last.test2_s_default;
  Alcotest.(check (list int)) "Last conf 2 final" Last.v_2_3 [42; 69]

let test_set = [
  ("last 0", `Quick, test_last_0);
  ("last 1", `Quick, test_last_1);
  ("last 2", `Quick, test_last_2)
]