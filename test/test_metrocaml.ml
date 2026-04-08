open Metrocaml

let dummy_station : Metrocaml.Network.station =
  { id = "0"; name = "Dummy"; latitude = 0.0; longitude = 0.0; pmr = false }

(* ========================================================== *)
(* TESTS: HopWeight                                           *)
(* ========================================================== *)

module HW = Metrocaml.Weight.HopWeight

let hop_weight_t =
  Alcotest.testable (fun ppf w -> Fmt.string ppf (HW.to_string w)) HW.( = )

let force_traverse traverse_fn w e =
  match traverse_fn w e with
  | Some (new_w, _) -> new_w
  | None -> Alcotest.fail "Traverse a renvoyé None (chemin bloqué)"

let test_hop_of_edge () =
  let dummy_line : Metrocaml.Network.line =
    { name = "Ligne 1"; color = "#FFBE00" }
  in
  let dummy_conn : Metrocaml.Network.connection =
    {
      line = dummy_line;
      duration = 120;
      distance = 500.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let expected = force_traverse HW.traverse (HW.start_at 0) dummy_conn in
  Alcotest.(check string)
    "Un segment vaut 1 arrêt" "1 arrêts (Aujourd'hui à 00h02m00s)"
    (HW.to_string expected)

let test_hop_arithmetic () =
  let dummy_conn =
    {
      Metrocaml.Network.line = { name = ""; color = "" };
      duration = 0;
      distance = 0.;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let one = force_traverse HW.traverse (HW.start_at 0) dummy_conn in
  let two = force_traverse HW.traverse one dummy_conn in
  Alcotest.(check string)
    "1 + 1 = 2" "2 arrêts (Aujourd'hui à 00h00m00s)" (HW.to_string two);
  Alcotest.(check hop_weight_t)
    "Vérifie l'accumulation" two
    (force_traverse HW.traverse one dummy_conn)

let test_hop_infinity () =
  let dummy_conn =
    {
      Metrocaml.Network.line = { name = ""; color = "" };
      duration = 0;
      distance = 0.;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  Alcotest.(check bool)
    "L'infini empêche la traversée (None)" true
    (HW.traverse HW.infinity dummy_conn = None)

(* ========================================================== *)
(* TESTS: TimeWeight                                          *)
(* ========================================================== *)

module TimeW = Metrocaml.Weight.TimeWeight

let time_weight_t =
  Alcotest.testable
    (fun ppf w -> Fmt.string ppf (TimeW.to_string w))
    TimeW.( = )

let test_time_of_edge () =
  let dummy_line : Metrocaml.Network.line =
    { name = "Ligne 1"; color = "#FFBE00" }
  in
  let dummy_conn : Metrocaml.Network.connection =
    {
      line = dummy_line;
      duration = 120;
      distance = 500.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  Alcotest.(check string)
    "Extraction de la duree" "Aujourd'hui à 00h02m00s"
    (TimeW.to_string
       (force_traverse TimeW.traverse (TimeW.start_at 0) dummy_conn))

let test_time_arithmetic () =
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let c1 =
    {
      Metrocaml.Network.line = dummy_line;
      duration = 60;
      distance = 0.;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let c2 =
    {
      Metrocaml.Network.line = dummy_line;
      duration = 40;
      distance = 0.;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let c_100 =
    {
      Metrocaml.Network.line = dummy_line;
      duration = 100;
      distance = 0.;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let w1 = force_traverse TimeW.traverse (TimeW.start_at 0) c1 in
  let w2 = force_traverse TimeW.traverse w1 c2 in
  let expected_100 = force_traverse TimeW.traverse (TimeW.start_at 0) c_100 in
  Alcotest.(check time_weight_t) "60 + 40 = 100" expected_100 w2

let test_time_compare () =
  let open TimeW in
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let w_60 =
    force_traverse traverse (start_at 0)
      {
        line = dummy_line;
        duration = 60;
        distance = 0.;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  let w_120 =
    force_traverse traverse (start_at 0)
      {
        line = dummy_line;
        duration = 120;
        distance = 0.;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  Alcotest.(check bool) "Moins de temps gagne (<)" true (w_60 < w_120);
  Alcotest.(check bool) "Plus de temps perd (>)" true (w_120 > w_60);
  Alcotest.(check bool) "Temps egaux (=)" true (w_60 = w_60)

let test_time_to_string () =
  Alcotest.(check string)
    "Formatage 0s" "Aujourd'hui à 00h00m00s"
    (TimeW.to_string (TimeW.start_at 0));
  Alcotest.(check string)
    "Formatage minutes exactes (2m00s)" "Aujourd'hui à 00h02m00s"
    (TimeW.to_string (TimeW.start_at 120))

(* ========================================================== *)
(* TESTS: DistanceWeight                                      *)
(* ========================================================== *)

module DW = Metrocaml.Weight.DistanceWeight

let distance_weight_t =
  Alcotest.testable (fun ppf w -> Fmt.string ppf (DW.to_string w)) DW.( = )

let test_distance_of_edge () =
  let dummy_line : Metrocaml.Network.line =
    { name = "Ligne 1"; color = "#FFBE00" }
  in
  let dummy_conn : Metrocaml.Network.connection =
    {
      line = dummy_line;
      duration = 120;
      distance = 1500.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  Alcotest.(check string)
    "Extraction de la distance" "1.5 km (Aujourd'hui à 00h02m00s)"
    (DW.to_string (force_traverse DW.traverse (DW.start_at 0) dummy_conn))

let test_distance_arithmetic () =
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let conn1 =
    {
      Metrocaml.Network.line = dummy_line;
      duration = 60;
      distance = 400.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let conn2 =
    {
      Metrocaml.Network.line = dummy_line;
      duration = 60;
      distance = 800.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let w1 = force_traverse DW.traverse (DW.start_at 0) conn1 in
  let w2 = force_traverse DW.traverse w1 conn2 in
  Alcotest.(check string)
    "400m + 800m = 1.2 km" "1.2 km (Aujourd'hui à 00h02m00s)" (DW.to_string w2)

let test_distance_tie_breaker () =
  let open DW in
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let w_fast =
    force_traverse traverse (start_at 0)
      {
        line = dummy_line;
        duration = 60;
        distance = 500.0;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  let w_slow =
    force_traverse traverse (start_at 0)
      {
        line = dummy_line;
        duration = 300;
        distance = 500.0;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  Alcotest.(check bool)
    "A distance egale, le temps le plus court gagne (<)" true (w_fast < w_slow);
  Alcotest.(check bool)
    "A distance egale, le temps le plus long perd (>)" true (w_slow > w_fast)

let test_distance_infinity () =
  let dummy_conn =
    {
      Metrocaml.Network.line = { name = ""; color = "" };
      duration = 0;
      distance = 100.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  Alcotest.(check bool)
    "L'infini empêche la traversée (None)" true
    (DW.traverse DW.infinity dummy_conn = None)

(* ========================================================== *)
(* TESTS: CorrespondenceWeight                                *)
(* ========================================================== *)

module CW = Metrocaml.Weight.CorrespondenceWeight

let test_cw_transfer_penalty () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let line_b = { Metrocaml.Network.name = "Ligne B"; color = "#0000FF" } in
  let conn_a : Metrocaml.Network.connection =
    {
      line = line_a;
      duration = 60;
      distance = 100.;
      mode = Metro;
      schedules = [ ("x", 0); ("y", 60); ("z", 120) ];
      terminus = dummy_station;
    }
  in
  let conn_b : Metrocaml.Network.connection =
    {
      line = line_b;
      duration = 60;
      distance = 100.;
      mode = Metro;
      schedules =
        [ ("x", 0); ("y", 60); ("z", 120); ("w", 180); ("v", 240); ("u", 300) ];
      terminus = dummy_station;
    }
  in

  let w_a = force_traverse CW.traverse (CW.start_at 0) conn_a in
  let total = force_traverse CW.traverse w_a conn_b in
  Alcotest.(check string)
    "Changement de ligne : +1 correspondance"
    "1 correspondances (Aujourd'hui à 00h05m00s)" (CW.to_string total)

let test_cw_no_transfer () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let conn1 =
    {
      Metrocaml.Network.line = line_a;
      duration = 60;
      distance = 100.;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let conn2 =
    {
      Metrocaml.Network.line = line_a;
      duration = 60;
      distance = 100.;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let w1 = force_traverse CW.traverse (CW.start_at 0) conn1 in
  let w2 = force_traverse CW.traverse w1 conn2 in
  Alcotest.(check string)
    "Meme ligne : 0 correspondance"
    "0 correspondances (Aujourd'hui à 00h02m00s)" (CW.to_string w2)

let test_cw_tie_breaker () =
  let open CW in
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  (* Tous deux ont 0 correspondance, mais l'un est plus rapide *)
  let w_fast =
    force_traverse traverse (start_at 0)
      {
        line = line_a;
        duration = 60;
        distance = 100.;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  let w_slow =
    force_traverse traverse (start_at 0)
      {
        line = line_a;
        duration = 300;
        distance = 100.;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  Alcotest.(check bool)
    "A correspondances egales, le temps le plus court gagne (<)" true
    (w_fast < w_slow)

let test_cw_infinity () =
  let conn =
    {
      Metrocaml.Network.line = { name = ""; color = "" };
      duration = 0;
      distance = 100.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  Alcotest.(check bool)
    "L'infini empêche la traversée (None)" true
    (CW.traverse CW.infinity conn = None)

(* ========================================================== *)
(* TESTS: BFS (Parcours en largeur)                           *)
(* ========================================================== *)

module BFS_Tests = struct
  module TestNode = struct
    type t = string

    let equal = String.equal
    let hash = Hashtbl.hash
    let compare = String.compare
  end

  module TestEdge = struct
    type t = Metrocaml.Network.connection

    let compare = compare

    let default =
      {
        Metrocaml.Network.line = { name = ""; color = "" };
        duration = 0;
        distance = 0.;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  end

  module TestGraph = Routing.Graph.Make (TestNode) (TestEdge)
  module BFS = Routing.Bfs.Make (TestGraph) (HW)

  let dummy_conn : Metrocaml.Network.connection =
    {
      line = { name = "Test"; color = "#000000" };
      duration = 60;
      distance = 1000.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }

  let test_bfs_simple_path () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    TestGraph.add_node g "B";
    TestGraph.add_edge g "A" "B" dummy_conn;

    match BFS.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B" with
    | Some path ->
        Alcotest.(check int) "Chemin direct (1 saut)" 1 (List.length path.steps)
    | None -> Alcotest.fail "Un chemin direct A->B devrait exister"
end

(* ========================================================== *)
(* TESTS: Dijkstra                                            *)
(* ========================================================== *)

module Dijkstra_Tests = struct
  module TestNode = BFS_Tests.TestNode
  module TestEdge = BFS_Tests.TestEdge
  module TestGraph = Routing.Graph.Make (TestNode) (TestEdge)
  module TW = Metrocaml.Weight.TimeWeight
  module Dijkstra = Routing.Dijkstra.Make (TestGraph) (TW)

  let conn dur : Metrocaml.Network.connection =
    {
      line = { name = "Test"; color = "#000000" };
      duration = dur;
      distance = 0.;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }

  let test_simple_path () =
    let g = TestGraph.create () in
    TestGraph.add_edge g "A" "B" (conn 60);
    match
      Dijkstra.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B"
    with
    | Some path ->
        Alcotest.(check int)
          "Chemin direct (1 étape)" 1 (List.length path.steps)
    | None -> Alcotest.fail "Un chemin direct A->B devrait exister"

  let test_weighted_optimal () =
    let g = TestGraph.create () in
    TestGraph.add_edge g "A" "C" (conn 100);
    TestGraph.add_edge g "A" "B" (conn 10);
    TestGraph.add_edge g "B" "C" (conn 10);
    match
      Dijkstra.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"C"
    with
    | None -> Alcotest.fail "Chemin A->C introuvable"
    | Some path ->
        Alcotest.(check int)
          "Chemin optimal: 2 étapes (via B)" 2 (List.length path.steps)
end

(* ========================================================== *)
(* MAIN RUNNER                                                *)
(* ========================================================== *)

let () =
  let open Alcotest in
  run "MetroCaml Suite"
    [
      ( "HopWeight",
        [
          test_case "Poids unitaire" `Quick test_hop_of_edge;
          test_case "Arithmétique" `Quick test_hop_arithmetic;
          test_case "Gestion Infini" `Quick test_hop_infinity;
        ] );
      ( "TimeWeight",
        [
          test_case "Extraction de la durée" `Quick test_time_of_edge;
          test_case "Arithmétique" `Quick test_time_arithmetic;
          test_case "Comparaison booléenne" `Quick test_time_compare;
          test_case "Formatage de l'affichage" `Quick test_time_to_string;
        ] );
      ( "DistanceWeight",
        [
          test_case "Extraction de la distance" `Quick test_distance_of_edge;
          test_case "Arithmétique" `Quick test_distance_arithmetic;
          test_case "Tie-breaker temporel" `Quick test_distance_tie_breaker;
          test_case "Gestion Infini" `Quick test_distance_infinity;
        ] );
      ( "CorrespondenceWeight",
        [
          test_case "Pénalité de changement" `Quick test_cw_transfer_penalty;
          test_case "Maintien sur la ligne" `Quick test_cw_no_transfer;
          test_case "Tie-breaker temporel" `Quick test_cw_tie_breaker;
          test_case "Gestion Infini" `Quick test_cw_infinity;
        ] );
      ( "BFS",
        [ test_case "Chemin simple" `Quick BFS_Tests.test_bfs_simple_path ] );
      ( "Dijkstra",
        [
          test_case "Chemin simple" `Quick Dijkstra_Tests.test_simple_path;
          test_case "Chemin optimal pondéré" `Quick
            Dijkstra_Tests.test_weighted_optimal;
        ] );
    ]
