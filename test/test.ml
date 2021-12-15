let () =
    Alcotest.run "Rml ppx" [
      ("Unit tests", Test1.test_set);
      ("Unit tests signals", Signal.test_set);
      ("Depth search", Depth_search.test_set);
      ("Default", Default.test_set);
      ("Await_when", Await.test_set);
      ("Pre", Pre.test_set);
      ("Last", Last.test_set)
    ]