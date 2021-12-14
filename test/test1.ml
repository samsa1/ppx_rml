[%%rml.Rififi
  let c =
  let a = 1 and b = 2 in a + b;;
  let%signal test0_s = { default = None; gather = (fun x y -> Some x)};;
  let%signal test2_s = { default = None; gather = (fun x y -> Some x)};;
  let%signal test1_s = { default = Some 1; gather = (fun x y -> Some x)};;
  emit test0_s 1;;
  emit test2_s 'c';;
  let d = ref 0;;
  if%present (test0_s /| test0_s) then d := 42 else ();;
]

let test_rififi () =
  Alcotest.(check int) "let/and" !(Rififi.d) 42

let test_set = [
  ("rififi", `Quick, test_rififi)
]