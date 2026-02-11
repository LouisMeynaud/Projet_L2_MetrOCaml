open Metrocaml

module HW = Metrocaml.Weight.HopWeight 

let hop_weight_t = 
  Alcotest.testable (fun ppf w -> Fmt.string ppf (HW.to_string w)) HW.(=)

let test_hop_of_edge () =
  let dummy_line : Metrocaml.Network.line = { name = "Ligne 1"; color = "#FFBE00" } in
  
  let dummy_conn : Metrocaml.Network.connection = { 
    line = dummy_line; 
    duration = 120; 
    distance = 500.0; 
    mode = Metro 
  } in 
  
  let expected = HW.of_edge dummy_conn in 
  
  Alcotest.(check hop_weight_t) "Un segment vaut bien la valeur unitaire" 
    expected (HW.of_edge dummy_conn)

    let test_hop_arithmetic () =
      let open HW in
      let dummy_conn = { Metrocaml.Network.line = {name=""; color=""}; duration=0; distance=0.; mode=Walk } in
      let one = of_edge dummy_conn in
      let two = add one one in
      
      Alcotest.(check hop_weight_t) "1 + 1 = 2 hops" two (add one one);
      
      Alcotest.(check hop_weight_t) "Zéro est neutre" one (add one zero)

      let test_hop_infinity () =
        let open HW in
        let dummy_conn = { Metrocaml.Network.line = {name=""; color=""}; duration=0; distance=0.; mode=Walk } in
        let one = of_edge dummy_conn in
        
        Alcotest.(check hop_weight_t) "Infini + 1 = Infini" infinity (add infinity one);
        Alcotest.(check hop_weight_t) "Infini + Infini = Infini" infinity (add infinity infinity)

let () =
  let open Alcotest in
  run "MetroCaml Suite" [
    "HopWeight", [
      test_case "Poids unitaire" `Quick test_hop_of_edge;
      test_case "Arithmétique" `Quick test_hop_arithmetic;
      test_case "Infini" `Quick test_hop_infinity;
    ];
  ]
