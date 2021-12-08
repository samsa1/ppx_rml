module type S = sig end

[%%rml
  let a = 1 and b = 2 in print_int (a + b)
]