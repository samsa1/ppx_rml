[%%rml.Rififi
  let c =
  let a = 1 and b = 2 in a + b;;
  let%signal test0_s = { default = None; gather = (fun x y -> Some x)};;
  let%signal test1_s = { default = None; gather = (fun x y -> Some x)};;
  let%signal test2_s = { default = None; gather = (fun x y -> Some x)};;
  emit test0_s 1;;
  emit test2_s 2;;
  let d = if%present z = test0_s then 42 else 69;;
]

let test_rififi () =
  Alcotest.(check int) "let/and" Rififi.d 42

let test_set = [
  ("rififi", `Quick, test_rififi)
]