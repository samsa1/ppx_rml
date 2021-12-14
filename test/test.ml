let () =
    Alcotest.run "Rml ppx" [
      ("Unit tests", Test1.test_set);
      ("Depth search", Depth_search.test_set)
    ]