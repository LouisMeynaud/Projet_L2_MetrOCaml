let dummy_test () =
  (* dummy test to ensure Alcotest and the CI are working *)
  Alcotest.(check int) "same int" 42 42


let () =
  let open Alcotest in
  run "Tests" [
    "dummy_test", [
      test_case "Dummy test" `Quick dummy_test;
    ];
  ]
