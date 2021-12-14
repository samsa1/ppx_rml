[%%rml.Rififi
  let c =
  let a = 1 and b = 2 in a + b;;
  let%signal test0_s = { default = None; gather = (fun x _ -> Some x)};;
  let%signal test1_s = { default = None; gather = (fun x _ -> Some x)};;
  emit test0_s 1;;
  emit test1_s 2;;
  let d = if%present test0_s then 42 else 69;;
]

let test_rififi () =
  Alcotest.(check int) "let/and" Rififi.d 42

let test_set = [
  ("rififi", `Quick, test_rififi)
]