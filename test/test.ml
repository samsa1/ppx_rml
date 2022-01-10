let () =
    Alcotest.run "Rml ppx" [
      ("Unit tests", Test1.test_set);
      ("Unit tests signals", Signal.test_set);
      ("Depth search", Depth_search.test_set);
      ("Default", Default.test_set);
      ("Await_when", Await.test_set);
      ("Await_rml", Await2test.test_set);
      ("Pre", Pre.test_set);
      ("Last", Last.test_set);
      ("Outside deps", Ocaml_ppx.test_set);
      ("Control", Control.test_set);
      ("Do When", Do_when.test_set);
    ]