let rec fac n =
  if n = 0 then 1 else n * fac (n - 1)

let fibo n =
  let rec f n =
    if n = 0 then (1, 1)
    else let (a, b) = f (n - 1) in (b, a + b)
  in fst (f n) 

[%%rml.Ocaml

  let fac5 : int = [%ocaml fac] 5

  let fac10 : int = [%ocaml fac] 10

  let fibo5 : int = [%ocaml fibo] 5

  let fibo10 : int = [%ocaml fibo] 10

]

let test_fac () =
  Alcotest.(check int) "Factorial 5" Ocaml.fac5 120; 
  Alcotest.(check int) "Factorial 10" Ocaml.fac10 (fac 10)

let test_fibo () =
  Alcotest.(check int) "Fibo 5" Ocaml.fibo5 8; 
  Alcotest.(check int) "Fibo 10" Ocaml.fibo10 89


let test_set = [
  ("factorial", `Quick, test_fac);
  ("fibonacci", `Quick, test_fibo);
]