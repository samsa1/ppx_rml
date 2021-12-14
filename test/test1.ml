[%%rml.Rififi
  let c =
  let a = 1 and b = 2 in a + b
]

let test_rififi () =
  Alcotest.(check int) "let/and" Rififi.c 3

let test_set = [
  ("rififi", `Quick, test_rififi)
]