[%%rml.Rififi
  let c =
  let a = 1 and b = 2 in a + b
]

let _ = 
  assert (Rififi.c = 3)