[%%rml.Default
  let%signal test0_s = {default = 69; gather = (fun x _ -> x)};;
  let v_0_0 = default test0_s;;
  emit test0_s 42;;
  let v_0_1 = default test0_s;;

  let%signal test1_s = {default = []; gather = fun x y -> x :: y};;
  let v_1_0 = default test1_s;;
  emit test1_s 42;;
  let v_1_1 = default test1_s;;

  let%signal test2_s = {default = [69]; gather = fun x y -> x :: y};;
  let v_2_0 = default test2_s;;
  emit test2_s 42;;
  let v_2_1 = default test2_s;;

]

let test_default_0 () =
  Alcotest.(check int) "let/and" Default.v_0_0 69;
  Alcotest.(check int) "let/and" Default.v_0_1 69
  
let test_default_1 () =
  Alcotest.(check (list int)) "let/and" Default.v_1_0 [];
  Alcotest.(check (list int)) "let/and" Default.v_1_1 []

let test_default_2 () =
  Alcotest.(check (list int)) "let/and" Default.v_2_0 [69];
  Alcotest.(check (list int)) "let/and" Default.v_2_1 [69]

let test_set = [
  ("default 0", `Quick, test_default_0);
  ("default 1", `Quick, test_default_1);
  ("default 2", `Quick, test_default_2)
]