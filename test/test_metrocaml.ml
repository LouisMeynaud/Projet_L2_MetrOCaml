open Metrocaml

(* ========================================================== *)
(* TESTS: HopWeight                                           *)
(* ========================================================== *)

module HW = Metrocaml.Weight.HopWeight 

let hop_weight_t = 
  Alcotest.testable (fun ppf w -> Fmt.string ppf (HW.to_string w)) HW.(=)

let test_hop_of_edge () =
  let dummy_line : Metrocaml.Network.line = { name = "Ligne 1"; color = "#FFBE00" } in
  let dummy_conn : Metrocaml.Network.connection = { 
    line = dummy_line; duration = 120; distance = 500.0; mode = Metro 
  } in 
  let expected = HW.of_edge dummy_conn in 
  Alcotest.(check string) "Un segment vaut 1" "1 hop" (HW.to_string expected)

let test_hop_arithmetic () =
  let open HW in
  let dummy_conn = { Metrocaml.Network.line = {name=""; color=""}; duration=0; distance=0.; mode=Walk } in
  let one = of_edge dummy_conn in
  let two = HW.add one one in
  Alcotest.(check string) "1 + 1 = 2" "2 hops" (HW.to_string two);
  Alcotest.(check hop_weight_t) "Neutre" one (HW.add one zero)

let test_hop_infinity () =
  let open HW in
  let dummy_conn = { Metrocaml.Network.line = {name=""; color=""}; duration=0; distance=0.; mode=Walk } in
  let one = of_edge dummy_conn in
  Alcotest.(check string) "Infini absorbe" "∞" (HW.to_string (HW.add infinity one))

(* ========================================================== *)
(* TESTS: TransferWeight                                      *)
(* ========================================================== *)

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
  Alcotest.(check string) "Cumul stops et temps" "2 stops, 1m40s" (TW.to_string expected_sum)

let test_transfer_compare1 () =
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let path_a = TW.add (TW.of_edge {line=dummy_line; duration=250; distance=0.; mode=Walk}) 
                      (TW.of_edge {line=dummy_line; duration=250; distance=0.; mode=Walk}) in
  let path_b = TW.add (TW.of_edge {line=dummy_line; duration=50; distance=0.; mode=Walk})
                      (TW.add (TW.of_edge {line=dummy_line; duration=25; distance=0.; mode=Walk})
                              (TW.of_edge {line=dummy_line; duration=25; distance=0.; mode=Walk})) in
  
  Alcotest.(check bool) "Lexicographique: moins d'arrêts gagne" true (TW.(path_a < path_b))

let test_transfer_compare2 () =
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let path_a = TW.add (TW.of_edge {line=dummy_line; duration=250; distance=0.; mode=Walk}) 
                      (TW.of_edge {line=dummy_line; duration=250; distance=0.; mode=Walk}) in
  let path_b = TW.add (TW.of_edge {line=dummy_line; duration=50; distance=0.; mode=Walk})
                      (TW.of_edge {line=dummy_line; duration=25; distance=0.; mode=Walk}) in
    
    Alcotest.(check bool) "Lexicographique: moins de temps gagne" true (TW.(path_b < path_a))

let test_transfer_to_string () =
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let w = TW.of_edge {line=dummy_line; duration=3661; distance=0.; mode=Walk} in
  Alcotest.(check string) "Formatage h/m/s" "1 stops, 1h01m01s" (TW.to_string w)


(* ========================================================== *)
(* TESTS: CorrespondenceWeight                                *)
(* ========================================================== *)

module CW = Metrocaml.Weight.CorrespondenceWeight

let correspondence_weight_t = 
  Alcotest.testable (fun ppf w -> Fmt.string ppf (CW.to_string w)) CW.(=)

let test_cw_of_edge () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let conn = { 
    Metrocaml.Network.line = line_a; duration = 120; distance = 500.0; mode = Metro 
  } in 
  let weight = CW.of_edge conn in
  (* of_edge doit retourner (0, Some "Ligne A") *)
  Alcotest.(check string) "Poids initial (0 correspondance)" "0 correspondance(s)" (CW.to_string weight)

let test_cw_arithmetic_same_line () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let conn = { Metrocaml.Network.line = line_a; duration = 60; distance = 100.; mode = Metro } in
  let w1 = CW.of_edge conn in
  let w2 = CW.of_edge conn in
  let sum = CW.add w1 w2 in
  Alcotest.(check string) "Même ligne : pas de pénalité" "0 correspondance(s)" (CW.to_string sum)

let test_cw_transfer_penalty () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let line_b = { Metrocaml.Network.name = "Ligne B"; color = "#0000FF" } in
  let w_a = CW.of_edge { line = line_a; duration = 60; distance = 100.; mode = Metro } in
  let w_b = CW.of_edge { line = line_b; duration = 60; distance = 100.; mode = Metro } in
  
  let total = CW.add w_a w_b in
  Alcotest.(check string) "Changement de ligne : +1 correspondance" "1 correspondance(s)" (CW.to_string total);
  
  let total_3_lines = CW.add total (CW.of_edge { line = {name="Ligne C"; color=""}; duration=0; distance=0.; mode=Metro }) in
  Alcotest.(check string) "Deux changements : 2m" "2 correspondance(s)" (CW.to_string total_3_lines)

let test_cw_neutral_and_inf () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let w_a = CW.of_edge { line = line_a; duration = 60; distance = 100.; mode = Metro } in
  
  Alcotest.(check correspondence_weight_t) "Neutre à gauche" w_a (CW.add CW.zero w_a);
  Alcotest.(check correspondence_weight_t) "Neutre à droite" w_a (CW.add w_a CW.zero);
  Alcotest.(check string) "Infini est absorbant" "∞" (CW.to_string (CW.add CW.infinity w_a))

let test_cw_compare () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "" } in
  let line_b = { Metrocaml.Network.name = "Ligne B"; color = "" } in
  
  let w_no_transfer = CW.add (CW.of_edge {line=line_a; duration=0; distance=0.; mode=Metro}) 
                             (CW.of_edge {line=line_a; duration=0; distance=0.; mode=Metro}) in
  let w_one_transfer = CW.add (CW.of_edge {line=line_a; duration=0; distance=0.; mode=Metro}) 
                              (CW.of_edge {line=line_b; duration=0; distance=0.; mode=Metro}) in
  
  Alcotest.(check bool) "0 correspondance < 1 correspondance" true (CW.(w_no_transfer < w_one_transfer));
  Alcotest.(check bool) "1 correspondance > 0 correspondance" true (CW.(w_one_transfer > w_no_transfer))

(* ========================================================== *)
(* TESTS: BFS (Parcours en largeur)                           *)
(* ========================================================== *)

module BFS_Tests = struct
  (* Définition des nœuds et arêtes pour le foncteur Graph *)
  module TestNode = struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
    let compare = String.compare
  end
  
  module TestEdge = struct
    type t = Metrocaml.Network.connection
    let compare = compare
    let default = { 
      Metrocaml.Network.line = { name=""; color="" }; 
      duration = 0; 
      distance = 0.; 
      mode = Walk 
    }
  end

  (* Utilisation du VRAI module Graph de la librairie *)
  module TestGraph = Routing.Graph.Make(TestNode)(TestEdge)
  
  (* Création du BFS spécialisé pour ce graphe et HopWeight *)
  module BFS = Routing.Bfs.Make(TestGraph)(HW)

  (* Données de test *)
  let dummy_conn : Metrocaml.Network.connection = {
    line = { name = "Test"; color = "#000000" }; 
    duration = 60; 
    distance = 1000.0; 
    mode = Metro
  }

  (* Tests *)

  let test_bfs_simple_path () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    TestGraph.add_node g "B";
    TestGraph.add_edge g "A" "B" dummy_conn;

    match BFS.find_path ~graph:g ~source:"A" ~destination:"B" with
    | Some path -> Alcotest.(check int) "Chemin direct (1 saut)" 1 (List.length path.steps)
    | None -> Alcotest.fail "Un chemin direct A->B devrait exister"

  let test_bfs_no_path () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    TestGraph.add_node g "B";
    
    match BFS.find_path ~graph:g ~source:"A" ~destination:"B" with
    | Some _ -> Alcotest.fail "Aucun chemin ne devrait être trouvé"
    | None -> ()

  let test_bfs_same_node () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    
    match BFS.find_path ~graph:g ~source:"A" ~destination:"A" with
    | Some path -> Alcotest.(check int) "Chemin vers soi-même (0 saut)" 0 (List.length path.steps)
    | None -> Alcotest.fail "Le chemin A->A devrait exister"

  let test_bfs_missing_source () =
    let g = TestGraph.create () in
    TestGraph.add_node g "B";
    match BFS.find_path ~graph:g ~source:"A" ~destination:"B" with
    | Some _ -> Alcotest.fail "Devrait échouer car A n'existe pas"
    | None -> ()

  let test_bfs_longer_path () =
    let g = TestGraph.create () in
    List.iter (TestGraph.add_node g) ["A"; "B"; "C"; "D"];
    (* Chaîne A -> B -> C -> D *)
    TestGraph.add_edge g "A" "B" dummy_conn;
    TestGraph.add_edge g "B" "C" dummy_conn;
    TestGraph.add_edge g "C" "D" dummy_conn;

    match BFS.find_path ~graph:g ~source:"A" ~destination:"D" with
    | Some path -> Alcotest.(check int) "Chemin A->B->C->D (3 sauts)" 3 (List.length path.steps)
    | None -> Alcotest.fail "Chemin chainé non trouvé"

  let test_bfs_cycle () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    TestGraph.add_node g "B";
    (* Cycle A <-> B *)
    TestGraph.add_edge g "A" "B" dummy_conn;
    TestGraph.add_edge g "B" "A" dummy_conn;

    match BFS.find_path ~graph:g ~source:"A" ~destination:"B" with
    | Some path -> Alcotest.(check int) "Gère les cycles (1 saut)" 1 (List.length path.steps)
    | None -> Alcotest.fail "Devrait trouver B même avec un cycle retour"
end

(* ========================================================== *)
(* TESTS: Dijkstra                                            *)
(* ========================================================== *)

module Dijkstra_Tests = struct
  module TestNode = struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
    let compare = String.compare
  end

  module TestEdge = struct
    type t = Metrocaml.Network.connection
    let compare = compare
    let default = {
      Metrocaml.Network.line = { name = ""; color = "" };
      duration = 0; distance = 0.; mode = Walk
    }
  end

  module TestGraph = Routing.Graph.Make(TestNode)(TestEdge)

  module TW = Metrocaml.Weight.TimeWeight
  module Dijkstra = Routing.Dijkstra.Make(TestGraph)(TW)

  let time_weight_t =
    Alcotest.testable (fun ppf w -> Fmt.string ppf (TW.to_string w)) TW.(=)

  let conn dur : Metrocaml.Network.connection =
    { line = { name = "Test"; color = "#000000" };
      duration = dur; distance = 0.; mode = Metro }

  (* --- Tests --- *)

  let test_simple_path () =
    let g = TestGraph.create () in
    TestGraph.add_edge g "A" "B" (conn 60);
    match Dijkstra.find_path ~graph:g ~source:"A" ~destination:"B" with
    | Some path ->
        Alcotest.(check int) "Chemin direct (1 étape)" 1 (List.length path.steps)
    | None -> Alcotest.fail "Un chemin direct A->B devrait exister"

  let test_no_path () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    TestGraph.add_node g "B";
    match Dijkstra.find_path ~graph:g ~source:"A" ~destination:"B" with
    | Some _ -> Alcotest.fail "Aucun chemin ne devrait être trouvé"
    | None -> ()

  let test_same_node () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    match Dijkstra.find_path ~graph:g ~source:"A" ~destination:"A" with
    | Some path -> Alcotest.(check int) "Source = destination (0 étape)" 0 (List.length path.steps)
    | None -> Alcotest.fail "Le chemin A->A devrait exister"

  let test_missing_source () =
    let g = TestGraph.create () in
    TestGraph.add_node g "B";
    match Dijkstra.find_path ~graph:g ~source:"A" ~destination:"B" with
    | Some _ -> Alcotest.fail "Devrait échouer car A n'existe pas"
    | None -> ()

  let test_missing_destination () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    match Dijkstra.find_path ~graph:g ~source:"A" ~destination:"B" with
    | Some _ -> Alcotest.fail "Devrait échouer car B n'existe pas"
    | None -> ()

  let test_chain () =
    let g = TestGraph.create () in
    List.iter (TestGraph.add_node g) ["A"; "B"; "C"; "D"];
    TestGraph.add_edge g "A" "B" (conn 10);
    TestGraph.add_edge g "B" "C" (conn 10);
    TestGraph.add_edge g "C" "D" (conn 10);
    match Dijkstra.find_path ~graph:g ~source:"A" ~destination:"D" with
    | Some path -> Alcotest.(check int) "Chaîne A->B->C->D (3 étapes)" 3 (List.length path.steps)
    | None -> Alcotest.fail "Chemin chaîné non trouvé"

  let test_cycle () =
    let g = TestGraph.create () in
    TestGraph.add_edge g "A" "B" (conn 10);
    TestGraph.add_edge g "B" "A" (conn 10);
    match Dijkstra.find_path ~graph:g ~source:"A" ~destination:"B" with
    | Some path -> Alcotest.(check int) "Gère les cycles (1 étape)" 1 (List.length path.steps)
    | None -> Alcotest.fail "Devrait trouver B même en présence d'un cycle"

  let test_weighted_optimal () =
    let g = TestGraph.create () in
    TestGraph.add_edge g "A" "C" (conn 100); (* chemin direct, coûteux *)
    TestGraph.add_edge g "A" "B" (conn 10);
    TestGraph.add_edge g "B" "C" (conn 10);  (* chemin indirect, moins coûteux *)
    match Dijkstra.find_path ~graph:g ~source:"A" ~destination:"C" with
    | None -> Alcotest.fail "Chemin A->C introuvable"
    | Some path ->
        Alcotest.(check int) "Chemin optimal: 2 étapes (via B)" 2 (List.length path.steps);
        Alcotest.(check time_weight_t) "Poids optimal: 20s"
          (TW.add (TW.of_edge (conn 10)) (TW.of_edge (conn 10)))
          path.total_weight

  let test_total_weight () =
    let g = TestGraph.create () in
    TestGraph.add_edge g "A" "B" (conn 30);
    TestGraph.add_edge g "B" "C" (conn 45);
    match Dijkstra.find_path ~graph:g ~source:"A" ~destination:"C" with
    | None -> Alcotest.fail "Chemin non trouvé"
    | Some path ->
        Alcotest.(check time_weight_t) "Poids total A->B->C = 75s"
          (TW.add (TW.of_edge (conn 30)) (TW.of_edge (conn 45)))
          path.total_weight

end

(* ========================================================== *)
(* MAIN RUNNER                                                *)
(* ========================================================== *)

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
      test_case "Priorité lexicographique" `Quick test_transfer_compare1;
      test_case "Priorité lexicographique" `Quick test_transfer_compare2;
      test_case "Formatage temps" `Quick test_transfer_to_string;
    ];
    "CorrespondenceWeight", [
      test_case "Poids initial" `Quick test_cw_of_edge;
      test_case "Arithmétique et pénalité" `Quick test_cw_arithmetic_same_line;
      test_case "Pénalité de correspondance" `Quick test_cw_transfer_penalty;
      test_case "Neutre et infini" `Quick test_cw_neutral_and_inf;
      test_case "Comparaison" `Quick test_cw_compare;
    ];
    "BFS", [
      test_case "Chemin simple" `Quick BFS_Tests.test_bfs_simple_path;
      test_case "Pas de chemin" `Quick BFS_Tests.test_bfs_no_path;
      test_case "Même nœud" `Quick BFS_Tests.test_bfs_same_node;
      test_case "Source manquante" `Quick BFS_Tests.test_bfs_missing_source;
      test_case "Chemin long" `Quick BFS_Tests.test_bfs_longer_path;
      test_case "Cycle" `Quick BFS_Tests.test_bfs_cycle;
    ];
    "Dijkstra", [
      test_case "Chemin simple" `Quick Dijkstra_Tests.test_simple_path;
      test_case "Pas de chemin" `Quick Dijkstra_Tests.test_no_path;
      test_case "Même nœud" `Quick Dijkstra_Tests.test_same_node;
      test_case "Source manquante" `Quick Dijkstra_Tests.test_missing_source;
      test_case "Destination manquante" `Quick Dijkstra_Tests.test_missing_destination;
      test_case "Chaîne" `Quick Dijkstra_Tests.test_chain;
      test_case "Cycle" `Quick Dijkstra_Tests.test_cycle;
      test_case "Chemin optimal pondéré" `Quick Dijkstra_Tests.test_weighted_optimal;
      test_case "Poids total" `Quick Dijkstra_Tests.test_total_weight;
    ];
  ]
