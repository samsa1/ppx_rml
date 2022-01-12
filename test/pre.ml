[%%rml.Pre
  let test0_s = Signal {default = 69; gather = (fun x _ -> x)};;
  let test0_s_default = default test0_s;;
  let v_0_0 = pre_status test0_s;;
  let v_0_1 = pre_value test0_s;;
  emit test0_s 42;;
  pause;;
  let v_0_2 = pre_status test0_s;;
  let v_0_3 = pre_value test0_s;;

  let test1_s = Signal {default = []; gather = fun x y -> x :: y};;
  let test1_s_default = default test1_s;;
  let v_1_0 = pre_status test1_s;;
  let v_1_1 = pre_value test1_s;;
  emit test1_s 42;;
  pause;;
  let v_1_2 = pre_status test1_s;;
  let v_1_3 = pre_value test1_s;;

  let test2_s = Signal {default = [69]; gather = fun x y -> x :: y};;
  let test2_s_default = default test2_s;;
  let v_2_0 = pre_status test2_s;;
  let v_2_1 = pre_value test2_s;;
  emit test2_s 42;;
  pause;;
  let v_2_2 = pre_status test2_s;;
  let v_2_3 = pre_value test2_s;;

]

let test_pre_0 () =
  Alcotest.(check bool) "Pre conf 0 status initial" Pre.v_0_0 false;
  Alcotest.(check int) "Pre conf 0 value initial" Pre.v_0_1 69; 
  Alcotest.(check int) "Pre conf 0 value default" Pre.v_0_1 Pre.test0_s_default;
  Alcotest.(check bool) "Pre conf 0 status final" Pre.v_0_2 true;
  Alcotest.(check int) "Pre conf 0 value final" Pre.v_0_3 42
  
let test_pre_1 () =
  Alcotest.(check bool) "Pre conf 1 status initial" Pre.v_1_0 false;
  Alcotest.(check (list int)) "Pre conf 1 value initial" Pre.v_1_1 []; 
  Alcotest.(check (list int)) "Pre conf 1 value default" Pre.v_1_1 Pre.test1_s_default;
  Alcotest.(check bool) "Pre conf 1 status final" Pre.v_1_2 true;
  Alcotest.(check (list int)) "Pre conf 1 value final" Pre.v_1_3 [42]

let test_pre_2 () =
  Alcotest.(check bool) "Pre conf 2 status initial" Pre.v_2_0 false;
  Alcotest.(check (list int)) "Pre conf 2 value initial" Pre.v_2_1 [69]; 
  Alcotest.(check (list int)) "Pre conf 2 value default" Pre.v_2_1 Pre.test2_s_default;
  Alcotest.(check bool) "Pre conf 2 status final" Pre.v_2_2 true;
  Alcotest.(check (list int)) "Pre conf 2 value final" Pre.v_2_3 [42; 69]

let test_set = [
  ("pre 0", `Quick, test_pre_0);
  ("pre 1", `Quick, test_pre_1);
  ("pre 2", `Quick, test_pre_2)
]