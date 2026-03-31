open Metrocaml

(* ========================================================== *)
(* TESTS: HopWeight                                           *)
(* ========================================================== *)

module HW = Metrocaml.Weight.HopWeight

let hop_weight_t =
  Alcotest.testable (fun ppf w -> Fmt.string ppf (HW.to_string w)) HW.( = )

let dummy_station : Metrocaml.Network.station =
  {
    id = "La Station crée par Thomas(il est trop beau)";
    name = "StationTest";
    latitude = 3.14159;
    longitude = 1.41421;
    pmr = true;
  }

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
  let expected = HW.add (HW.start_at 0) dummy_conn in
  Alcotest.(check string)
    "Un segment vaut 1 arrêt" "1 arrêts (Aujourd'hui à 00h02m00s)"
    (HW.to_string expected)

let test_hop_arithmetic () =
  let open HW in
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
  let one = add (start_at 0) dummy_conn in
  let two = add one dummy_conn in
  Alcotest.(check string)
    "1 + 1 = 2" "2 arrêts (Aujourd'hui à 00h00m00s)" (HW.to_string two);
  Alcotest.(check hop_weight_t)
    "Vérifie l'accumulation" two (add one dummy_conn)

let test_hop_infinity () =
  let open HW in
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
  Alcotest.(check string)
    "Infini absorbe l'arête" "∞"
    (HW.to_string (HW.add infinity dummy_conn))

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
    (TimeW.to_string (TimeW.add (TimeW.start_at 0) dummy_conn))

let test_time_arithmetic () =
  let open TimeW in
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
  let w1 = add (start_at 0) c1 in
  let w2 = add w1 c2 in
  let expected_100 = add (start_at 0) c_100 in
  Alcotest.(check time_weight_t) "60 + 40 = 100" expected_100 w2

let test_time_compare () =
  let open TimeW in
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let w_60 =
    add (start_at 0)
      {
        Metrocaml.Network.line = dummy_line;
        duration = 60;
        distance = 0.;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  let w_120 =
    add (start_at 0)
      {
        Metrocaml.Network.line = dummy_line;
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

let test_time_infinity () =
  let open TimeW in
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let w_3600 =
    add (start_at 0)
      {
        Metrocaml.Network.line = dummy_line;
        duration = 3600;
        distance = 0.;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  Alcotest.(check bool)
    "L'infini est strictement superieur a une duree normale" true
    (infinity > w_3600);
  Alcotest.(check string) "Affichage de l'infini" "∞" (to_string infinity)

let test_time_to_string () =
  Alcotest.(check string)
    "Formatage 0s" "Aujourd'hui à 00h00m00s"
    (TimeW.to_string (TimeW.start_at 0));
  Alcotest.(check string)
    "Formatage secondes (45s)" "Aujourd'hui à 00h00m45s"
    (TimeW.to_string (TimeW.start_at 45));
  Alcotest.(check string)
    "Formatage minutes exactes (2m00s)" "Aujourd'hui à 00h02m00s"
    (TimeW.to_string (TimeW.start_at 120));
  Alcotest.(check string)
    "Formatage minutes et secondes (2m05s)" "Aujourd'hui à 00h02m05s"
    (TimeW.to_string (TimeW.start_at 125));
  Alcotest.(check string)
    "Formatage h/m/s (1h01m05s)" "Aujourd'hui à 01h01m05s"
    (TimeW.to_string (TimeW.start_at 3665))

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
    (DW.to_string (DW.add (DW.start_at 0) dummy_conn))

let test_distance_arithmetic () =
  let open DW in
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let c1 =
    {
      Metrocaml.Network.line = dummy_line;
      duration = 0;
      distance = 500.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let c2 =
    {
      Metrocaml.Network.line = dummy_line;
      duration = 0;
      distance = 750.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let c_1250 =
    {
      Metrocaml.Network.line = dummy_line;
      duration = 0;
      distance = 1250.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let w1 = add (start_at 0) c1 in
  let w2 = add w1 c2 in
  let expected_1250 = add (start_at 0) c_1250 in
  Alcotest.(check distance_weight_t) "500.0 + 750.0 = 1250.0" expected_1250 w2

let test_distance_compare () =
  let open DW in
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let w_500 =
    add (start_at 0)
      {
        Metrocaml.Network.line = dummy_line;
        duration = 0;
        distance = 500.0;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  let w_750 =
    add (start_at 0)
      {
        Metrocaml.Network.line = dummy_line;
        duration = 0;
        distance = 750.0;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  Alcotest.(check bool) "Moins de distance gagne (<)" true (w_500 < w_750);
  Alcotest.(check bool) "Plus de distance perd (>)" true (w_750 > w_500);
  Alcotest.(check bool) "Distances egales (=)" true (w_500 = w_500)

let test_distance_infinity () =
  let open DW in
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let dummy_station : Metrocaml.Network.station =
    { id = ""; name = ""; latitude = 1.; longitude = 1.; pmr = true }
  in
  let dummy_conn =
    {
      Metrocaml.Network.line = dummy_line;
      duration = 0;
      distance = 500.0;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }
  in
  let w_500 = add (start_at 0) dummy_conn in
  Alcotest.(check distance_weight_t)
    "L'infini absorbe l'addition (inf + x = inf)" infinity
    (add infinity dummy_conn);
  Alcotest.(check bool) "L'infini est superieur a tout" true (infinity > w_500);
  Alcotest.(check string) "Affichage de l'infini" "∞" (to_string infinity)

let test_distance_to_string () =
  let dummy_line : Metrocaml.Network.line = { name = ""; color = "" } in
  let make_w dist =
    DW.add (DW.start_at 0)
      {
        Metrocaml.Network.line = dummy_line;
        duration = 0;
        distance = dist;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
  in
  Alcotest.(check string)
    "Formatage 0 m" "0 m (Aujourd'hui à 00h00m00s)"
    (DW.to_string (make_w 0.0));
  Alcotest.(check string)
    "Formatage m (< 1000)" "850 m (Aujourd'hui à 00h00m00s)"
    (DW.to_string (make_w 850.0));
  Alcotest.(check string)
    "Formatage km exact (1000)" "1.0 km (Aujourd'hui à 00h00m00s)"
    (DW.to_string (make_w 1000.0));
  Alcotest.(check string)
    "Formatage km avec virgule" "1.2 km (Aujourd'hui à 00h00m00s)"
    (DW.to_string (make_w 1200.0))

(* ========================================================== *)
(* TESTS: CorrespondenceWeight                                *)
(* ========================================================== *)

module CW = Metrocaml.Weight.CorrespondenceWeight

let correspondence_weight_t =
  Alcotest.testable (fun ppf w -> Fmt.string ppf (CW.to_string w)) CW.( = )

let test_cw_of_edge () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let dummy_station : Metrocaml.Network.station =
    { id = ""; name = ""; latitude = 1.; longitude = 1.; pmr = true }
  in
  let conn : Metrocaml.Network.connection =
    {
      line = line_a;
      duration = 120;
      distance = 500.0;
      mode = Metro;
      schedules = [ 0; 120; 240 ];
      terminus = dummy_station;
    }
  in
  let weight = CW.add (CW.start_at 0) conn in
  Alcotest.(check string)
    "Poids initial (0 correspondance)"
    "0 correspondances (Aujourd'hui à 00h02m00s)" (CW.to_string weight)

let test_cw_arithmetic_same_line () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let dummy_station : Metrocaml.Network.station =
    { id = ""; name = ""; latitude = 1.; longitude = 1.; pmr = true }
  in
  let conn : Metrocaml.Network.connection =
    {
      line = line_a;
      duration = 60;
      distance = 100.;
      mode = Metro;
      schedules = [ 0; 60; 120; 180 ];
      terminus = dummy_station;
    }
  in
  let w1 = CW.add (CW.start_at 0) conn in
  let w2 = CW.add w1 conn in
  Alcotest.(check string)
    "Même ligne : pas de pénalité" "0 correspondances (Aujourd'hui à 00h02m00s)"
    (CW.to_string w2)

let test_cw_transfer_penalty () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let line_b = { Metrocaml.Network.name = "Ligne B"; color = "#0000FF" } in
  let dummy_station : Metrocaml.Network.station =
    { id = ""; name = ""; latitude = 1.; longitude = 1.; pmr = true }
  in
  let conn_a : Metrocaml.Network.connection =
    {
      line = line_a;
      duration = 60;
      distance = 100.;
      mode = Metro;
      schedules = [ 0; 60; 120 ];
      terminus = dummy_station;
    }
  in
  let conn_b : Metrocaml.Network.connection =
    {
      line = line_b;
      duration = 60;
      distance = 100.;
      mode = Metro;
      schedules = [ 0; 60; 120; 180; 240; 300 ];
      terminus = dummy_station;
    }
  in

  let w_a = CW.add (CW.start_at 0) conn_a in
  let total = CW.add w_a conn_b in
  Alcotest.(check string)
    "Changement de ligne : +1 correspondance"
    "1 correspondances (Aujourd'hui à 00h05m00s)" (CW.to_string total);

  let conn_c : Metrocaml.Network.connection =
    {
      line = { name = "Ligne C"; color = "" };
      duration = 0;
      distance = 0.;
      mode = Metro;
      schedules = [ 300; 480; 600 ];
      terminus = dummy_station;
    }
  in
  let total_3_lines = CW.add total conn_c in
  Alcotest.(check string)
    "Deux changements : 2 correspondances"
    "2 correspondances (Aujourd'hui à 00h08m00s)"
    (CW.to_string total_3_lines)

let test_cw_neutral_and_inf () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "#FF0000" } in
  let dummy_station : Metrocaml.Network.station =
    { id = ""; name = ""; latitude = 1.; longitude = 1.; pmr = true }
  in
  let conn_a : Metrocaml.Network.connection =
    {
      line = line_a;
      duration = 60;
      distance = 100.;
      mode = Metro;
      schedules = [ 0; 60; 120 ];
      terminus = dummy_station;
    }
  in
  Alcotest.(check string)
    "Infini est absorbant" "∞"
    (CW.to_string (CW.add CW.infinity conn_a))

let test_cw_compare () =
  let line_a = { Metrocaml.Network.name = "Ligne A"; color = "" } in
  let line_b = { Metrocaml.Network.name = "Ligne B"; color = "" } in
  let dummy_station : Metrocaml.Network.station =
    { id = ""; name = ""; latitude = 1.; longitude = 1.; pmr = true }
  in

  let conn_a : Metrocaml.Network.connection =
    {
      line = line_a;
      duration = 0;
      distance = 0.;
      mode = Metro;
      schedules = [ 0; 100; 200; 500 ];
      terminus = dummy_station;
    }
  in
  let conn_b : Metrocaml.Network.connection =
    {
      line = line_b;
      duration = 0;
      distance = 0.;
      mode = Metro;
      schedules = [ 0; 100; 200; 500 ];
      terminus = dummy_station;
    }
  in

  let w_no_transfer = CW.add (CW.add (CW.start_at 0) conn_a) conn_a in
  let w_one_transfer = CW.add (CW.add (CW.start_at 0) conn_a) conn_b in

  Alcotest.(check bool)
    "0 correspondance < 1 correspondance" true
    CW.(w_no_transfer < w_one_transfer);
  Alcotest.(check bool)
    "1 correspondance > 0 correspondance" true
    CW.(w_one_transfer > w_no_transfer)

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

  (* Tests *)

  let test_bfs_simple_path () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    TestGraph.add_node g "B";
    TestGraph.add_edge g "A" "B" dummy_conn;

    match BFS.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B" with
    | Some path ->
        Alcotest.(check int) "Chemin direct (1 saut)" 1 (List.length path.steps)
    | None -> Alcotest.fail "Un chemin direct A->B devrait exister"

  let test_bfs_no_path () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    TestGraph.add_node g "B";

    match BFS.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B" with
    | Some _ -> Alcotest.fail "Aucun chemin ne devrait être trouvé"
    | None -> ()

  let test_bfs_same_node () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";

    match BFS.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"A" with
    | Some path ->
        Alcotest.(check int)
          "Chemin vers soi-même (0 saut)" 0 (List.length path.steps)
    | None -> Alcotest.fail "Le chemin A->A devrait exister"

  let test_bfs_missing_source () =
    let g = TestGraph.create () in
    TestGraph.add_node g "B";
    match BFS.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B" with
    | Some _ -> Alcotest.fail "Devrait échouer car A n'existe pas"
    | None -> ()

  let test_bfs_longer_path () =
    let g = TestGraph.create () in
    List.iter (TestGraph.add_node g) [ "A"; "B"; "C"; "D" ];
    TestGraph.add_edge g "A" "B" dummy_conn;
    TestGraph.add_edge g "B" "C" dummy_conn;
    TestGraph.add_edge g "C" "D" dummy_conn;

    match BFS.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"D" with
    | Some path ->
        Alcotest.(check int)
          "Chemin A->B->C->D (3 sauts)" 3 (List.length path.steps)
    | None -> Alcotest.fail "Chemin chainé non trouvé"

  let test_bfs_cycle () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    TestGraph.add_node g "B";
    TestGraph.add_edge g "A" "B" dummy_conn;
    TestGraph.add_edge g "B" "A" dummy_conn;

    match BFS.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B" with
    | Some path ->
        Alcotest.(check int)
          "Gère les cycles (1 saut)" 1 (List.length path.steps)
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
  module TW = Metrocaml.Weight.TimeWeight
  module Dijkstra = Routing.Dijkstra.Make (TestGraph) (TW)

  let time_weight_t =
    Alcotest.testable (fun ppf w -> Fmt.string ppf (TW.to_string w)) TW.( = )

  let conn dur : Metrocaml.Network.connection =
    {
      line = { name = "Test"; color = "#000000" };
      duration = dur;
      distance = 0.;
      mode = Walk;
      schedules = [];
      terminus = dummy_station;
    }

  (* --- Tests --- *)

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

  let test_no_path () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    TestGraph.add_node g "B";
    match
      Dijkstra.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B"
    with
    | Some _ -> Alcotest.fail "Aucun chemin ne devrait être trouvé"
    | None -> ()

  let test_same_node () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    match
      Dijkstra.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"A"
    with
    | Some path ->
        Alcotest.(check int)
          "Source = destination (0 étape)" 0 (List.length path.steps)
    | None -> Alcotest.fail "Le chemin A->A devrait exister"

  let test_missing_source () =
    let g = TestGraph.create () in
    TestGraph.add_node g "B";
    match
      Dijkstra.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B"
    with
    | Some _ -> Alcotest.fail "Devrait échouer car A n'existe pas"
    | None -> ()

  let test_missing_destination () =
    let g = TestGraph.create () in
    TestGraph.add_node g "A";
    match
      Dijkstra.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B"
    with
    | Some _ -> Alcotest.fail "Devrait échouer car B n'existe pas"
    | None -> ()

  let test_chain () =
    let g = TestGraph.create () in
    List.iter (TestGraph.add_node g) [ "A"; "B"; "C"; "D" ];
    TestGraph.add_edge g "A" "B" (conn 10);
    TestGraph.add_edge g "B" "C" (conn 10);
    TestGraph.add_edge g "C" "D" (conn 10);
    match
      Dijkstra.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"D"
    with
    | Some path ->
        Alcotest.(check int)
          "Chaîne A->B->C->D (3 étapes)" 3 (List.length path.steps)
    | None -> Alcotest.fail "Chemin chaîné non trouvé"

  let test_cycle () =
    let g = TestGraph.create () in
    TestGraph.add_edge g "A" "B" (conn 10);
    TestGraph.add_edge g "B" "A" (conn 10);
    match
      Dijkstra.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"B"
    with
    | Some path ->
        Alcotest.(check int)
          "Gère les cycles (1 étape)" 1 (List.length path.steps)
    | None -> Alcotest.fail "Devrait trouver B même en présence d'un cycle"

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
          "Chemin optimal: 2 étapes (via B)" 2 (List.length path.steps);
        Alcotest.(check time_weight_t)
          "Poids optimal: 20s"
          (TW.add (TW.add (TW.start_at 0) (conn 10)) (conn 10))
          path.total_weight

  let test_total_weight () =
    let g = TestGraph.create () in
    TestGraph.add_edge g "A" "B" (conn 30);
    TestGraph.add_edge g "B" "C" (conn 45);
    match
      Dijkstra.find_path ~start_time:0 ~graph:g ~source:"A" ~destination:"C"
    with
    | None -> Alcotest.fail "Chemin non trouvé"
    | Some path ->
        Alcotest.(check time_weight_t)
          "Poids total A->B->C = 75s"
          (TW.add (TW.add (TW.start_at 0) (conn 30)) (conn 45))
          path.total_weight
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
          test_case "Gestion de l'Infini" `Quick test_time_infinity;
          test_case "Formatage de l'affichage" `Quick test_time_to_string;
        ] );
      ( "DistanceWeight",
        [
          test_case "Extraction de la distance" `Quick test_distance_of_edge;
          test_case "Arithmétique" `Quick test_distance_arithmetic;
          test_case "Comparaison booléenne" `Quick test_distance_compare;
          test_case "Gestion de l'Infini" `Quick test_distance_infinity;
          test_case "Formatage de l'affichage" `Quick test_distance_to_string;
        ] );
      ( "CorrespondenceWeight",
        [
          test_case "Poids initial" `Quick test_cw_of_edge;
          test_case "Arithmétique et pénalité" `Quick
            test_cw_arithmetic_same_line;
          test_case "Pénalité de correspondance" `Quick test_cw_transfer_penalty;
          test_case "Neutre et infini" `Quick test_cw_neutral_and_inf;
          test_case "Comparaison" `Quick test_cw_compare;
        ] );
      ( "BFS",
        [
          test_case "Chemin simple" `Quick BFS_Tests.test_bfs_simple_path;
          test_case "Pas de chemin" `Quick BFS_Tests.test_bfs_no_path;
          test_case "Même nœud" `Quick BFS_Tests.test_bfs_same_node;
          test_case "Source manquante" `Quick BFS_Tests.test_bfs_missing_source;
          test_case "Chemin long" `Quick BFS_Tests.test_bfs_longer_path;
          test_case "Cycle" `Quick BFS_Tests.test_bfs_cycle;
        ] );
      ( "Dijkstra",
        [
          test_case "Chemin simple" `Quick Dijkstra_Tests.test_simple_path;
          test_case "Pas de chemin" `Quick Dijkstra_Tests.test_no_path;
          test_case "Même nœud" `Quick Dijkstra_Tests.test_same_node;
          test_case "Source manquante" `Quick Dijkstra_Tests.test_missing_source;
          test_case "Destination manquante" `Quick
            Dijkstra_Tests.test_missing_destination;
          test_case "Chaîne" `Quick Dijkstra_Tests.test_chain;
          test_case "Cycle" `Quick Dijkstra_Tests.test_cycle;
          test_case "Chemin optimal pondéré" `Quick
            Dijkstra_Tests.test_weighted_optimal;
          test_case "Poids total" `Quick Dijkstra_Tests.test_total_weight;
        ] );
    ]
