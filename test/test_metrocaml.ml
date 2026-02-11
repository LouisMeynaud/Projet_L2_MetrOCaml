open Metrocaml


module HW = Metrocaml.Weight.HopWeight 

let hop_weight_t = 
  Alcotest.testable (fun ppf w -> Fmt.string ppf (HW.to_string w)) HW.(=)

let test_hop_of_edge () =
  let dummy_line : Metrocaml.Network.line = { name = "Ligne 1"; color = "#FFBE00" } in
  let dummy_conn : Metrocaml.Network.connection = { 
    line = dummy_line; duration = 120; distance = 500.0; mode = Metro 
  } in 
  let expected = HW.of_edge dummy_conn in 
  Alcotest.(check hop_weight_t) "Un segment vaut 1" expected (HW.of_edge dummy_conn)

let test_hop_arithmetic () =
  let open HW in
  let dummy_conn = { Metrocaml.Network.line = {name=""; color=""}; duration=0; distance=0.; mode=Walk } in
  let one = of_edge dummy_conn in
  let two = add one one in
  Alcotest.(check hop_weight_t) "1 + 1 = 2" two (add one one);
  Alcotest.(check hop_weight_t) "Neutre" one (add one zero)

let test_hop_infinity () =
  let open HW in
  let dummy_conn = { Metrocaml.Network.line = {name=""; color=""}; duration=0; distance=0.; mode=Walk } in
  let one = of_edge dummy_conn in
  Alcotest.(check hop_weight_t) "Infini absorbe" infinity (add infinity one)


module TW = Metrocaml.Weight.TransferWeight

let transfer_weight_t = 
  Alcotest.testable (fun ppf w -> Fmt.string ppf (TW.to_string w)) TW.(=)

let test_transfer_arithmetic () =
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let c1 = { Metrocaml.Network.line = dummy_line; duration = 60; distance = 0.; mode = Walk } in
  let c2 = { Metrocaml.Network.line = dummy_line; duration = 40; distance = 0.; mode = Walk } in
  let w1 = TW.of_edge c1 in
  let w2 = TW.of_edge c2 in
  let expected_sum = TW.add w1 w2 in
  Alcotest.(check transfer_weight_t) "Cumul stops et temps" expected_sum (TW.add w1 w2)

let test_transfer_compare () =
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let path_a = TW.add (TW.of_edge {line=dummy_line; duration=250; distance=0.; mode=Walk}) 
                      (TW.of_edge {line=dummy_line; duration=250; distance=0.; mode=Walk}) in
  let path_b = TW.add (TW.of_edge {line=dummy_line; duration=50; distance=0.; mode=Walk})
                      (TW.add (TW.of_edge {line=dummy_line; duration=25; distance=0.; mode=Walk})
                              (TW.of_edge {line=dummy_line; duration=25; distance=0.; mode=Walk})) in
  
  Alcotest.(check bool) "Lexicographique: moins d'arrêts gagne" true (TW.(path_a < path_b))

let test_transfer_to_string () =
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let w = TW.of_edge {line=dummy_line; duration=3661; distance=0.; mode=Walk} in
  Alcotest.(check string) "Formatage h/m/s" "1 stops, 1h01m01s" (TW.to_string w)


let () =
  let open Alcotest in
  run "MetroCaml Suite" [
    "HopWeight", [
      test_case "Poids unitaire" `Quick test_hop_of_edge;
      test_case "Arithmétique" `Quick test_hop_arithmetic;
      test_case "Gestion Infini" `Quick test_hop_infinity;
    ];
    "TransferWeight", [
      test_case "Addition composite" `Quick test_transfer_arithmetic;
      test_case "Priorité lexicographique" `Quick test_transfer_compare;
      test_case "Formatage temps" `Quick test_transfer_to_string;
    ];
  ]
