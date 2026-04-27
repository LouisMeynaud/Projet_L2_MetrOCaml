open Metrocaml

let dummy_station : Metrocaml.Network.station =
  { id = "0"; name = "Dummy"; latitude = 0.0; longitude = 0.0; pmr = false }

(* ========================================================== *)
(* eval_edge                                                  *)
(* ========================================================== *)

module EvalEdge_Tests = struct
  open Metrocaml.Weight

  (** Initialisation du type de retour de eval_edge pour les comparaisons *)
  let eval_res_t =
    let pp_opt_str ppf = function
      | None -> Fmt.string ppf "None"
      | Some s -> Fmt.pf ppf "Some %s" s
    in
    let pp_tuple ppf (time, wait, line) =
      Fmt.pf ppf "(%d, %d, %a)" time wait pp_opt_str line
    in
    Alcotest.(option (testable pp_tuple ( = )))

  let line_A : Metrocaml.Network.line = { name = "A"; color = ""; id = "A" }
  let line_B : Metrocaml.Network.line = { name = "B"; color = ""; id = "B" }
  let line_C : Metrocaml.Network.line = { name = "C"; color = ""; id = "C" }

  (** Test de marche donc pas de contrainte de temps *)
  let test_eval_edge_walk () =
    let conn =
      {
        Metrocaml.Network.line = line_A;
        duration = 120;
        distance = 100.;
        mode = Walk;
        schedules = [];
        terminus = dummy_station;
      }
    in
    let res = eval_edge 1000 None dummy_station conn in
    Alcotest.(check eval_res_t)
      "Marche: ajoute juste la durée, pas d'attente, garde la ligne"
      (Some (1120, 0, None)) res

  (** Test si on ne change pas de ligne, avec vérification que la marge fonctionne bien *)
  let test_eval_edge_same_line () =
    let conn =
      {
        Metrocaml.Network.line = line_A;
        duration = 300;
        distance = 1000.;
        mode = Metro;
        schedules = [ ("id1", 900); ("id2", 1100); ("id3", 1300) ];
        terminus = dummy_station;
      }
    in
    let res = eval_edge 1150 (Some "A") dummy_station conn in
    Alcotest.(check eval_res_t)
      "Même ligne: profite de la marge pour attraper le train"
      (Some (1450, 0, Some "A")) res

  (** Test de l'ajout de pénalité en cas de correspondance *)
  let test_eval_edge_transfer () =
    let conn =
      {
        Metrocaml.Network.line = line_B;
        duration = 200;
        distance = 1000.;
        mode = Metro;
        schedules = [ ("id1", 1000); ("id2", 1100); ("id3", 1500) ];
        terminus = dummy_station;
      }
    in
    let res = eval_edge 1000 (Some "A") dummy_station conn in
    Alcotest.(check eval_res_t)
      "Transfert: applique la pénalité et trouve le bon train"
      (Some (1700, 500, Some "B")) res

  (** Test s'il n'y a plus de train disponible *)
  let test_eval_edge_missed_train () =
    let conn =
      {
        Metrocaml.Network.line = line_A;
        duration = 200;
        distance = 1000.;
        mode = Metro;
        schedules = [ ("id1", 500); ("id2", 600) ];
        terminus = dummy_station;
      }
    in
    let res = eval_edge 1000 None dummy_station conn in
    Alcotest.(check eval_res_t)
      "Train raté: renvoie None si plus de trains dans la journée"
      None res

  (** Test de l'ajout de pénalité en cas de perturbations sur la ligne *)
  let test_eval_edge_perturbed_entry () =
    let conn =
      {
        Metrocaml.Network.line = line_C;
        duration = 200;
        distance = 1000.;
        mode = Metro;
        schedules = [ ("id1", 1200); ("id2", 1800) ];
        terminus = dummy_station;
      }
    in
    let alerte : Metrocaml.Gtfs_rt.alerte_dijkstra =
      {
        severite = Metrocaml.Gtfs_rt.PERTURBEE;
        stations_hash = Hashtbl.create 0;
        periodes = [];
      }
    in
    Hashtbl.replace !Metrocaml.Gtfs_rt.index_alertes_dijkstra "C" [ alerte ];
    
    let res = eval_edge 1000 None dummy_station conn in
    
    Hashtbl.clear !Metrocaml.Gtfs_rt.index_alertes_dijkstra;
    
    Alcotest.(check eval_res_t)
      "Perturbation: ajoute 10min à la montée et fait rater le premier train"
      (Some (2000, 800, Some "C")) res

  (** Test si la ligne est bloquée *)
  let test_eval_edge_blocked () =
    let conn =
      {
        Metrocaml.Network.line = line_C;
        duration = 200;
        distance = 1000.;
        mode = Metro;
        schedules = [ ("id1", 2000) ];
        terminus = dummy_station;
      }
    in
    let alerte : Metrocaml.Gtfs_rt.alerte_dijkstra =
      {
        severite = Metrocaml.Gtfs_rt.BLOQUANTE;
        stations_hash = Hashtbl.create 0;
        periodes = [];
      }
    in
    Hashtbl.replace !Metrocaml.Gtfs_rt.index_alertes_dijkstra "C" [ alerte ];

    let res = eval_edge 1000 None dummy_station conn in

    Hashtbl.clear !Metrocaml.Gtfs_rt.index_alertes_dijkstra;

    Alcotest.(check eval_res_t)
      "Bloqué: la perturbation coupe la connexion (None)"
      None res
end


(* ========================================================== *)
(* TimeWeight                                                 *)
(* ========================================================== *)

module TimeW = Metrocaml.Weight.TimeWeight

let time_weight_t =
  Alcotest.testable
    (fun ppf w -> Fmt.string ppf (TimeW.to_string w))
    TimeW.( = )

(* --- Outils de création d'arêtes pour les tests --- *)
let make_line name = { Metrocaml.Network.name; color = ""; id = name }

let make_conn mode duration schedules line_name =
  {
    Metrocaml.Network.line = make_line line_name;
    duration;
    distance = 0.;
    mode;
    schedules;
    terminus = dummy_station;
  }

let force_traverse traverse_fn w e =
  match traverse_fn w dummy_station e with
  | Some (new_w, _) -> new_w
  | None -> Alcotest.fail "Traverse a renvoyé None (chemin bloqué inattendu)"

(** Test d'initialisation des données *)
let test_timew_init_states () =
  let start_w = TimeW.start_at 1000 in
  Alcotest.(check int) "start_at initialise le temps" 1000 (TimeW.get_time start_w);
  Alcotest.(check bool) "start_at n'est pas l'infini" false (TimeW.is_inf start_w);
  
  let inf_w = TimeW.infinity in
  Alcotest.(check bool) "infinity est reconnu comme l'infini" true (TimeW.is_inf inf_w)

(** Tests pour traverse *)

(** Test d'un mode de transport exclu *)
let test_timew_traverse_excluded_mode () =
  Metrocaml.Weight.set_excluded_modes ["Bus"];
  
  let conn = make_conn Bus 100 [] "B1" in
  let res = TimeW.traverse (TimeW.start_at 0) dummy_station conn in
  
  Metrocaml.Weight.set_excluded_modes []; 
  
  Alcotest.(check bool) "Mode exclu renvoie None (sans appeler eval_edge)" true (res = None)

(** Test valeur de sortie *)
let test_timew_traverse_transport () =
  let conn = make_conn Metro 120 [("id1", 50)] "M1" in
  let start_w = TimeW.start_at 0 in
  let next_w = force_traverse TimeW.traverse start_w conn in
  Alcotest.(check int) "Temps mis à jour (50 attente + 120 trajet)" 170 (TimeW.get_time next_w)

(** Test accumulations de Walk + incrémentation du temps de trajet en cas de Walk ou de Transfert *)
let test_timew_traverse_walk () =
  let conn_walk1 = make_conn Walk 100 [] "Marche" in
  let conn_walk2 = make_conn Walk 20 [] "Marche" in
  let conn_trans = make_conn Transfert 80 [] "Couloir" in
  
  let w_walk1 = force_traverse TimeW.traverse (TimeW.start_at 0) conn_walk1 in
  let w_walk2 = force_traverse TimeW.traverse w_walk1 conn_walk2 in
  let w_total = force_traverse TimeW.traverse w_walk2 conn_trans in
  
  Alcotest.(check int) "Temps total après 2 marches" 200 (TimeW.get_time w_total)

(** Tests des comparaisons *)

(** gestion de l'infini *)
let test_timew_compare_infinity () =
  let w_norm = TimeW.start_at 100 in
  let w_inf = TimeW.infinity in
  
  Alcotest.(check bool) "Infini = Infini" true (TimeW.(w_inf = w_inf));
  Alcotest.(check bool) "Normal < Infini" true (TimeW.(w_norm < w_inf));
  Alcotest.(check bool) "Infini > Normal" true (TimeW.(w_inf > w_norm))

(** Temps domine *)
let test_timew_compare_time_dominates () =
  let w1 = force_traverse TimeW.traverse (TimeW.start_at 0) (make_conn Walk 100 [] "W") in
  let w2 = force_traverse TimeW.traverse (TimeW.start_at 0) (make_conn Metro 200 [("id",0)] "M") in
  
  Alcotest.(check bool) "Le temps le plus court gagne, peu importe la marche" true (TimeW.(w1 < w2));
  Alcotest.(check bool) "Les opérateurs inversés fonctionnent" true (TimeW.(w2 > w1))

  (** Si temps égaux, marche départage *)
let test_timew_tie_breaker () =
  let w1 = force_traverse TimeW.traverse (TimeW.start_at 0) (make_conn Metro 500 [("id",0)] "M") in
  let w2 = force_traverse TimeW.traverse (TimeW.start_at 0) (make_conn Walk 500 [] "W") in
  
  Alcotest.(check bool) "À temps égal, celui qui a le moins de marche gagne" true (TimeW.(w1 < w2));
  Alcotest.(check bool) "Les opérateurs inversés fonctionnent" true (TimeW.(w2 > w1))

(** Cas d'égalité parfaite *)
let test_timew_perfect_tie () =
  let w1 = force_traverse TimeW.traverse (TimeW.start_at 0) (make_conn Walk 100 [] "W") in
  let w2 = force_traverse TimeW.traverse (TimeW.start_at 0) (make_conn Walk 100 [] "W") in
  Alcotest.(check bool) "Égalité parfaite renvoie =" true (TimeW.(w1 = w2))

(** Tests temps d'attente *)
let test_timew_formatters () =
  
  let conn_walk = make_conn Walk 120 [] "Marche" in
  let _, step_0 = 
    match TimeW.traverse (TimeW.start_at 0) dummy_station conn_walk with
    | Some res -> res
    | None -> Alcotest.fail "Traversée Walk a échoué"
  in
  
  let conn_metro = make_conn Metro 100 [("id", 120)] "M1" in
  let _, step_120 = 
    match TimeW.traverse (TimeW.start_at 0) dummy_station conn_metro with
    | Some res -> res
    | None -> Alcotest.fail "Traversée Metro a échoué"
  in
  
  Alcotest.(check string) "Attente de 0min n'affiche rien" "" (TimeW.step_data_to_string step_0);
  Alcotest.(check string) "Attente formatée en minutes" " [attente 2 min]" (TimeW.step_data_to_string step_120);
  Alcotest.(check string) "Formatage de la date à 0s" "Aujourd'hui à 00h00m00s" (TimeW.to_string (TimeW.start_at 0))


(* ========================================================== *)
(* DistanceWeight                                             *)
(* ========================================================== *)

module DW = Metrocaml.Weight.DistanceWeight

let distance_weight_t =
  Alcotest.testable (fun ppf w -> Fmt.string ppf (DW.to_string w)) DW.( = )

(** Tests des états de bases *)
let test_dw_init_states () =
  let start_w = DW.start_at 0 in
  Alcotest.(check bool) "start_at n'est pas infini" false (DW.is_inf start_w);
  Alcotest.(check bool) "infinity est reconnu" true (DW.is_inf DW.infinity)

(** Tests pour traverse *)

(** Test d'un mode de transport exclu *)
let test_dw_traverse_excluded_mode () =
  Metrocaml.Weight.set_excluded_modes ["Bus"];
  let conn = make_conn Bus 100 [] "B1" in
  let res = DW.traverse (DW.start_at 0) dummy_station conn in
  Metrocaml.Weight.set_excluded_modes [];
  Alcotest.(check bool) "Mode exclu renvoie None" true (res = None)

(** Test que la somme lors de l'ajout d'une nouvelle arête se fasse bien *)
let test_dw_traverse_accumulation () =
  let conn1 = { (make_conn Walk 60 [] "W") with distance = 400.5 } in
  let conn2 = { (make_conn Walk 60 [] "W") with distance = 800.0 } in
  
  let w1 = force_traverse DW.traverse (DW.start_at 0) conn1 in
  let w2 = force_traverse DW.traverse w1 conn2 in
  
  Alcotest.(check string) "Accumulation correcte des distances" 
    "1.2 km (Aujourd'hui à 00h02m00s)" (DW.to_string w2)

(** Tests des comparaisons *)

(** gestion de l'infini *)
let test_dw_compare_infinity () =
  let w_norm = force_traverse DW.traverse (DW.start_at 0) { (make_conn Walk 0 [] "W") with distance = 10. } in
  let w_inf = DW.infinity in
  Alcotest.(check bool) "Infini = Infini" true (DW.(w_inf = w_inf));
  Alcotest.(check bool) "Fini < Infini" true (DW.(w_norm < w_inf));
  Alcotest.(check bool) "Infini > Fini" true (DW.(w_inf > w_norm))

(** Vérifie que seule la distance départage, qu'importe le temps *)
let test_dw_compare_distance_dominates () =
  let w_court = force_traverse DW.traverse (DW.start_at 0) { (make_conn Walk 3600 [] "W") with distance = 500.0 } in
  let w_long = force_traverse DW.traverse (DW.start_at 0) { (make_conn Metro 120 [("id", 0)] "M") with distance = 1500.0 } in
  
  Alcotest.(check bool) "La distance la plus courte gagne, peu importe le temps" true (DW.(w_court < w_long));
  Alcotest.(check bool) "Les opérateurs inversés fonctionnent" true (DW.(w_long > w_court))

(** Cas d'égalité, on ignore toujours le temps *)
let test_dw_compare_tie () =
  let w1 = force_traverse DW.traverse (DW.start_at 0) { (make_conn Walk 60 [] "W") with distance = 500.0 } in
  let w2 = force_traverse DW.traverse (DW.start_at 0) { (make_conn Walk 120 [] "W") with distance = 500.0 } in
  Alcotest.(check bool) "Distances égales renvoient l'égalité parfaite" true (DW.(w1 = w2))

(** Test du formatage des données *)
let test_dw_formatters () =
  let w_m = force_traverse DW.traverse (DW.start_at 0) { (make_conn Walk 0 [] "W") with distance = 850.0 } in
  let w_km = force_traverse DW.traverse (DW.start_at 0) { (make_conn Walk 0 [] "W") with distance = 2500.0 } in
  
  Alcotest.(check string) "Affichage en mètres si < 1000m" "850 m (Aujourd'hui à 00h00m00s)" (DW.to_string w_m);
  Alcotest.(check string) "Affichage en km si >= 1000m" "2.5 km (Aujourd'hui à 00h00m00s)" (DW.to_string w_km);
  Alcotest.(check string) "Affichage de l'infini" "∞" (DW.to_string DW.infinity)

(* ========================================================== *)
(* HopWeight                                                  *)
(* ========================================================== *)

module HW = Metrocaml.Weight.HopWeight

let hop_weight_t =
  Alcotest.testable (fun ppf w -> Fmt.string ppf (HW.to_string w)) HW.( = )

(** Tests des états de bases *)
let test_hopw_init_states () =
  let start_w = HW.start_at 0 in
  Alcotest.(check bool) "start_at n'est pas infini" false (HW.is_inf start_w);
  Alcotest.(check bool) "infinity est reconnu" true (HW.is_inf HW.infinity)

(** Tests pour traverse *)

(** Test d'un mode de transport exclu *)
let test_hopw_traverse_excluded_mode () =
  Metrocaml.Weight.set_excluded_modes ["Bus"];
  let conn = make_conn Bus 100 [] "B1" in
  let res = HW.traverse (HW.start_at 0) dummy_station conn in
  Metrocaml.Weight.set_excluded_modes [];
  Alcotest.(check bool) "Mode exclu renvoie None" true (res = None)

(** Test que la somme lors de l'ajout d'une nouvelle arête se fasse bien *)
let test_hopw_traverse_accumulation () =
  let conn = make_conn Metro 60 [("id", 0)] "M1" in
  
  let w1 = force_traverse HW.traverse (HW.start_at 0) conn in
  let w2 = force_traverse HW.traverse w1 conn in
  let w3 = force_traverse HW.traverse w2 conn in
  
  Alcotest.(check string) "Accumulation compte bien +1 à chaque fois" 
    "3 arrêts (Aujourd'hui à 00h03m00s)" (HW.to_string w3)

(** Tests des comparaisons *)

(** gestion de l'infini *)
let test_hopw_compare_infinity () =
  let w_norm = force_traverse HW.traverse (HW.start_at 0) (make_conn Metro 60 [("id", 0)] "M1") in
  let w_inf = HW.infinity in
  Alcotest.(check bool) "Infini = Infini" true (HW.(w_inf = w_inf));
  Alcotest.(check bool) "Fini < Infini" true (HW.(w_norm < w_inf));
  Alcotest.(check bool) "Infini > Fini" true (HW.(w_inf > w_norm))

(** Vérifie que seul le nombre d'arrêts départage, qu'importe le temps *)
let test_hopw_compare_hops_dominates () =
  let w_peu = force_traverse HW.traverse (HW.start_at 0) (make_conn Metro 3600 [("id", 0)] "M1") in
  let w_bcp_1 = force_traverse HW.traverse (HW.start_at 0) (make_conn Metro 60 [("id", 0)] "M2") in
  let w_bcp_2 = force_traverse HW.traverse w_bcp_1 (make_conn Metro 60 [("id", 60)] "M2") in
  let w_bcp_3 = force_traverse HW.traverse w_bcp_2 (make_conn Metro 60 [("id", 120)] "M2") in
  
  Alcotest.(check bool) "Celui avec le moins d'arrêts gagne, peu importe le temps" true (HW.(w_peu < w_bcp_3));
  Alcotest.(check bool) "Les opérateurs inversés fonctionnent" true (HW.(w_bcp_3 > w_peu))

(** Cas d'égalité, on ignore toujours le temps *)
let test_hopw_compare_tie () =
  let w1 = force_traverse HW.traverse (HW.start_at 0) (make_conn Walk 60 [] "W") in
  let w2 = force_traverse HW.traverse (HW.start_at 0) (make_conn Walk 500 [] "W") in
  
  Alcotest.(check bool) "À nombre d'arrêts égal, c'est une égalité parfaite (Temps ignoré)" true (HW.(w1 = w2))

(** Test du formatage des données *)
let test_hopw_formatters () =
  let w_0 = HW.start_at 0 in
  let w_1 = force_traverse HW.traverse w_0 (make_conn Walk 120 [] "W") in
  
  Alcotest.(check string) "Affichage 0 arrêts" "0 arrêts (Aujourd'hui à 00h00m00s)" (HW.to_string w_0);
  Alcotest.(check string) "Affichage 1 arrêt" "1 arrêts (Aujourd'hui à 00h02m00s)" (HW.to_string w_1);
  Alcotest.(check string) "Affichage infini" "∞" (HW.to_string HW.infinity)


(* ========================================================== *)
(* CorrespondenceWeight                                       *)
(* ========================================================== *)

module CW = Metrocaml.Weight.CorrespondenceWeight

let cw_t =
  Alcotest.testable (fun ppf w -> Fmt.string ppf (CW.to_string w)) CW.( = )

(** Tests des états de bases *)
let test_cw_init_states () =
  let start_w = CW.start_at 0 in
  Alcotest.(check bool) "start_at n'est pas infini" false (CW.is_inf start_w);
  Alcotest.(check string) "Commence à 0" "0 correspondances (Aujourd'hui à 00h00m00s)" (CW.to_string start_w);
  Alcotest.(check bool) "infinity est reconnu" true (CW.is_inf CW.infinity)

(** Tests pour traverse *)

(** Test d'un mode de transport exclu *)
let test_cw_traverse_excluded_mode () =
  Metrocaml.Weight.set_excluded_modes ["Bus"];
  let conn = make_conn Bus 100 [] "B1" in
  let res = CW.traverse (CW.start_at 0) dummy_station conn in
  Metrocaml.Weight.set_excluded_modes []; 
  Alcotest.(check bool) "Mode exclu renvoie None" true (res = None)

(** Test quand on reste sur la même ligne *)
let test_cw_traverse_same_line () =
  let conn_m1_a = make_conn Metro 60 [("id", 100)] "M1" in
  let conn_m1_b = make_conn Metro 60 [("id", 200)] "M1" in
  
  let w_1 = force_traverse CW.traverse (CW.start_at 0) conn_m1_a in
  let w_2 = force_traverse CW.traverse w_1 conn_m1_b in
  
  Alcotest.(check string) "Rester sur M1 = 0 correspondance" 
    "0 correspondances (Aujourd'hui à 00h04m20s)" (CW.to_string w_2)

(** Test avec une correspondance *)
let test_cw_traverse_real_transfer () =
  let conn_m1 = make_conn Metro 60 [("id", 100)] "M1" in
  let conn_walk = make_conn Walk 120 [] "Couloir" in
  let conn_m2 = make_conn Metro 60 [("id", 1000)] "M2" in
  
  let w_m1 = force_traverse CW.traverse (CW.start_at 0) conn_m1 in
  let w_walk = force_traverse CW.traverse w_m1 conn_walk in
  let w_total = force_traverse CW.traverse w_walk conn_m2 in
  
  Alcotest.(check string) "Changement de M1 à M2 compte exactement 1 correspondance" 
    "1 correspondances (Aujourd'hui à 00h17m40s)" (CW.to_string w_total)

(** Tests des comparaisons *)

(** gestion de l'infini *)
let test_cw_compare_infinity () =
  let w_norm = force_traverse CW.traverse (CW.start_at 0) (make_conn Metro 60 [("id", 100)] "M1") in
  let w_inf = CW.infinity in
  Alcotest.(check bool) "Infini = Infini" true (CW.(w_inf = w_inf));
  Alcotest.(check bool) "Fini < Infini" true (CW.(w_norm < w_inf));
  Alcotest.(check bool) "Infini > Fini" true (CW.(w_inf > w_norm))

(** Vérifie que seul le nombre de correspondances départage, qu'importe le temps *)
let test_cw_compare_transfers_dominates () =
  let direct = force_traverse CW.traverse (CW.start_at 0) (make_conn Metro 3600 [("id", 100)] "M1") in
  let w1 = force_traverse CW.traverse (CW.start_at 0) (make_conn Metro 60 [("id", 100)] "M1") in
  let transfert = force_traverse CW.traverse w1 (make_conn Metro 60 [("id", 1000)] "M2") in
  
  Alcotest.(check bool) "0 correspondance gagne, même si le trajet prend plus de temps" 
    true (CW.(direct < transfert));
  Alcotest.(check bool) "Les opérateurs inversés fonctionnent" true (CW.(transfert > direct))

(** Cas d'égalité *)
let test_cw_compare_tie () =
  let w_rapide = force_traverse CW.traverse (CW.start_at 0) (make_conn Metro 60 [("id", 100)] "M1") in
  let w_lent = force_traverse CW.traverse (CW.start_at 0) (make_conn Metro 3600 [("id", 100)] "M1") in
  
  Alcotest.(check bool) "À correspondances égales, = renvoie true (temps ignoré par ce poids)" 
    true (CW.(w_rapide = w_lent))


(* ========================================================== *)
(* BFS                                                        *)
(* ========================================================== *)

module BFS_Tests = struct
  module TestNode = struct
    type t = Metrocaml.Network.station
    let equal a b = String.equal a.Metrocaml.Network.id b.Metrocaml.Network.id
    let hash a = Hashtbl.hash a.Metrocaml.Network.id
    let compare a b = String.compare a.Metrocaml.Network.id b.Metrocaml.Network.id
    let to_string a = a.Metrocaml.Network.name
  end

  module TestEdge = struct
    type t = Metrocaml.Network.connection
    let compare = compare
    let default = make_conn Walk 0 [] "D"
  end

  module TestGraph = Routing.Graph.Make (TestNode) (TestEdge)
  module BFS = Routing.Bfs.Make (TestGraph) (Metrocaml.Weight.HopWeight)

  (* Stations de test *)
  let st_A = { dummy_station with id = "A"; name = "A" }
  let st_B = { dummy_station with id = "B"; name = "B" }
  let st_C = { dummy_station with id = "C"; name = "C" }
  let st_D = { dummy_station with id = "D"; name = "D" }

  (** Cas de base *)
  (** Noeud manquant *)
  let test_bfs_missing_node () =
    let g = TestGraph.create () in
    TestGraph.add_node g st_A;
    let res = BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_B in
    Alcotest.(check bool) "Destination absente du graphe renvoie None" true (res = None)

  (** Noeud de dépard = noeud ciblé *)
  let test_bfs_same_node () =
    let g = TestGraph.create () in
    TestGraph.add_node g st_A;
    match BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_A with
    | None -> Alcotest.fail "Même noeud devrait renvoyer un chemin vide"
    | Some path ->
        Alcotest.(check int) "Chemin sur place = 0 étape" 0 (List.length path.steps)

  (** La topologie du graph *)
  (** Chemin simple (A -> b -> C) *)
  let test_bfs_simple_path () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (make_conn Walk 60 [] "W");
    TestGraph.add_edge g st_B st_C (make_conn Walk 60 [] "W");
    
    match BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C with
    | None -> Alcotest.fail "Chemin A->C introuvable"
    | Some path ->
        Alcotest.(check int) "Chemin A->B->C fait 2 étapes" 2 (List.length path.steps)

  (** Noeud inaccessible *)
  let test_bfs_unreachable () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (make_conn Walk 60 [] "W");
    TestGraph.add_node g st_C;
    
    let res = BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C in
    Alcotest.(check bool) "Noeud déconnecté renvoie None" true (res = None)

  (** Test qu'on a bien le chemin avec le moins de sauts, qu'importe le temps *)
  let test_bfs_ignores_weights () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (make_conn Metro 10 [("id", 0)] "M1");
    TestGraph.add_edge g st_B st_C (make_conn Metro 10 [("id", 10)] "M1");
    
    TestGraph.add_edge g st_A st_C (make_conn Walk 3600 [] "W");
    
    match BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C with
    | None -> Alcotest.fail "Chemin A->C introuvable"
    | Some path ->
        Alcotest.(check int) "BFS choisit toujours le chemin avec le MOINS d'arêtes (1 au lieu de 2)" 
          1 (List.length path.steps)

  (** Test des restrictions potentielles *)
  (** Restriction de mode de transport *)
  let test_bfs_excluded_mode () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (make_conn Bus 60 [] "B1");
    TestGraph.add_edge g st_A st_C (make_conn Metro 60 [("id2", 100)] "L2");
    TestGraph.add_edge g st_C st_B (make_conn Metro 60 [("id3", 200)] "L2");
    
    Metrocaml.Weight.set_excluded_modes ["Bus"];
    let path_blocked = Option.get (BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_B) in
    Metrocaml.Weight.set_excluded_modes [];
    
    Alcotest.(check int) "BFS utilise le détour pour éviter le bus (2 étapes)" 2 (List.length path_blocked.steps)
  
  (** Perturbations *)
  let test_bfs_perturbation_and_times () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (make_conn Metro 200 [("t1", 100); ("t2", 1000)] "L1");
  
    let path_normal = Option.get (BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_B) in
    Alcotest.(check int) "Temps normal (attrape le 1er train) = 300s" 
      300 (Metrocaml.Weight.HopWeight.get_time path_normal.total_weight);
  
    let alerte : Metrocaml.Gtfs_rt.alerte_dijkstra =
      { severite = Metrocaml.Gtfs_rt.PERTURBEE; stations_hash = Hashtbl.create 0; periodes = [] }
    in
    Hashtbl.replace !Metrocaml.Gtfs_rt.index_alertes_dijkstra "L1" [ alerte ];
  
    let path_perturbed = Option.get (BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_B) in
    Hashtbl.clear !Metrocaml.Gtfs_rt.index_alertes_dijkstra; 
  
    Alcotest.(check int) "Temps perturbé (rate le train, prend le 2ème) = 1200s" 
      1200 (Metrocaml.Weight.HopWeight.get_time path_perturbed.total_weight)

  (** Blocage *)
  let test_bfs_alternative_route_when_blocked () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (make_conn Metro 60 [("id1", 100)] "L1");
    TestGraph.add_edge g st_A st_C (make_conn Metro 60 [("id2", 100)] "L2");
    TestGraph.add_edge g st_C st_B (make_conn Metro 60 [("id3", 200)] "L2");
    
    (** Chemin normal *)
    let path_normal = Option.get (BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_B) in
    Alcotest.(check int) "Normalement, BFS prend le direct (1 étape)" 1 (List.length path_normal.steps);
    
    (** Avec blocage *)
    let alerte : Metrocaml.Gtfs_rt.alerte_dijkstra =
      { severite = Metrocaml.Gtfs_rt.BLOQUANTE; stations_hash = Hashtbl.create 0; periodes = [] }
    in
    Hashtbl.replace !Metrocaml.Gtfs_rt.index_alertes_dijkstra "L1" [ alerte ];
    
    let path_blocked = Option.get (BFS.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_B) in
    Hashtbl.clear !Metrocaml.Gtfs_rt.index_alertes_dijkstra;
    
    Alcotest.(check int) "Avec blocage, BFS contourne par le détour (2 étapes)" 2 (List.length path_blocked.steps)
end

(* ========================================================== *)
(* TESTS: Dijkstra (EXHAUSTIF AVEC STATE-SPACE ROUTING)       *)
(* ========================================================== *)

module Dijkstra_Tests = struct
  module TestNode = BFS_Tests.TestNode
  module TestEdge = BFS_Tests.TestEdge
  module TestGraph = Routing.Graph.Make (TestNode) (TestEdge)

  (* Module de poids dédié aux tests unitaires de Dijkstra *)
  module MockWeight = struct
    type t = { cost : int; line_name : string option }
    type edge = Metrocaml.Network.connection
    type node = Metrocaml.Network.station
    type step_data = int
    type state_key = string option

    let start_at time = { cost = time; line_name = None }
    let infinity = { cost = max_int; line_name = None }
    let is_inf w = w.cost = max_int

    let traverse w _v (e : edge) =
      (* On utilise duration = -1 pour simuler une arête bloquée *)
      if e.duration < 0 then None 
      else Some ({ cost = w.cost + e.duration; line_name = Some e.line.name }, e.duration)

    let compare w1 w2 = float_of_int (compare w1.cost w2.cost)
    let ( < ) a b = a.cost < b.cost
    let ( > ) a b = a.cost > b.cost
    let ( = ) a b = a.cost = b.cost
    let get_time w = w.cost
    let to_string w = string_of_int w.cost
    let step_data_to_string _ = ""
    let get_state_key w = w.line_name
  end

  module Dijkstra = Routing.Dijkstra.Make (TestGraph) (MockWeight)

  (* Stations et Arêtes de test *)
  let st_A = { dummy_station with id = "A"; name = "A" }
  let st_B = { dummy_station with id = "B"; name = "B" }
  let st_C = { dummy_station with id = "C"; name = "C" }

  let mock_conn dur line_name =
    { (make_conn Metro dur [] line_name) with duration = dur }

  (* Cas de base *)
  (** Chemin simple *)
  let test_dij_linear_path () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (mock_conn 60 "L1");
    TestGraph.add_edge g st_B st_C (mock_conn 60 "L1");
    
    match Dijkstra.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C ~transfer_allowed:(fun _ -> true) with
    | None -> Alcotest.fail "Chemin linéaire A-B-C introuvable"
    | Some path ->
        Alcotest.(check int) "Chemin linéaire : 2 étapes" 2 (List.length path.steps);
        Alcotest.(check int) "Coût cumulé correct (60+60)" 120 (MockWeight.get_time path.total_weight)

  (** Noeud manquant *)
  let test_dij_missing_node () =
    let g = TestGraph.create () in
    TestGraph.add_node g st_A;
    let res = Dijkstra.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_B ~transfer_allowed:(fun _ -> true) in
    Alcotest.(check bool) "Destination absente = None" true (res = None)

  (** Noeud de dépard = noeud ciblé *)
  let test_dij_same_node () =
    let g = TestGraph.create () in
    TestGraph.add_node g st_A;
    match Dijkstra.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_A ~transfer_allowed:(fun _ -> true) with
    | None -> Alcotest.fail "Même noeud doit renvoyer un chemin"
    | Some path -> Alcotest.(check int) "Chemin sur place = coût 0" 0 (MockWeight.get_time path.total_weight)

  (** Noeud inaccessible *)
  let test_dij_unreachable_node () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (mock_conn 60 "L1");
    TestGraph.add_node g st_C;
    
    let res = Dijkstra.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C ~transfer_allowed:(fun _ -> true) in
    Alcotest.(check bool) "Noeud totalement isolé renvoie None" true (res = None)

  (** Optimalité mathématiques *)
  (** Choix du chemin le moins cher *)
  let test_dij_optimal_path () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (mock_conn 10 "L1");
    TestGraph.add_edge g st_B st_C (mock_conn 10 "L1");
    TestGraph.add_edge g st_A st_C (mock_conn 100 "L2");

    match Dijkstra.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C ~transfer_allowed:(fun _ -> true) with
    | None -> Alcotest.fail "Chemin introuvable"
    | Some path -> 
        Alcotest.(check int) "Dijkstra préfère le détour moins coûteux (20 vs 100)" 20 (MockWeight.get_time path.total_weight)

  
  (** Test le cas où le meilleur chemin final n'est pas le meilleur à une étape intermédiaire *)
  let test_dij_state_space_routing () =
    let g = TestGraph.create () in
    
    TestGraph.add_edge g st_A st_B (mock_conn 50 "L1");
    TestGraph.add_edge g st_B st_C (mock_conn 10 "L1");

    TestGraph.add_edge g st_A st_B (mock_conn 5 "L2");
    TestGraph.add_edge g st_B st_C (mock_conn 90 "L2");

    let path_transfer_ok = Option.get (Dijkstra.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C ~transfer_allowed:(fun _ -> true)) in
    Alcotest.(check int) "Avec transferts autorisés, prend la ligne rapide puis change (Coût 15)" 
      15 (MockWeight.get_time path_transfer_ok.total_weight);

    let path_transfer_forbidden = Option.get (Dijkstra.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C ~transfer_allowed:(fun station -> station.Metrocaml.Network.id <> "B")) in
    Alcotest.(check int) "Avec transferts interdits, Dijkstra anticipe et prend la ligne lente dès le départ (Coût 60)" 
      60 (MockWeight.get_time path_transfer_forbidden.total_weight)
  
  (** Test des restrictions potentielles *)
  (** Perturbation *)
  let test_dij_perturbation_impact () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (mock_conn 10 "L1");
    TestGraph.add_edge g st_B st_C (mock_conn 110 "L1");
    
    TestGraph.add_edge g st_A st_C (mock_conn 50 "L2");

    match Dijkstra.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C ~transfer_allowed:(fun _ -> true) with
    | None -> Alcotest.fail "Chemin introuvable"
    | Some path ->
        Alcotest.(check int) "Dijkstra change d'itinéraire car la perturbation a rendu le chemin direct plus rentable (50)" 
          50 (MockWeight.get_time path.total_weight)

  (** Blocage *)
  let test_dij_rerouting_after_blockage () =
    let g = TestGraph.create () in
    TestGraph.add_edge g st_A st_B (mock_conn 10 "L2");
    TestGraph.add_edge g st_B st_C (mock_conn 90 "L2");

    TestGraph.add_edge g st_A st_C (mock_conn (-1) "L1"); 

    match Dijkstra.find_path ~start_time:0 ~graph:g ~source:st_A ~destination:st_C ~transfer_allowed:(fun _ -> true) with
    | None -> Alcotest.fail "Dijkstra devrait trouver le chemin de secours L2"
    | Some path ->
        Alcotest.(check int) "Dijkstra a ignoré le blocage et s'est rabattu sur L2 (Coût 100)" 
          100 (MockWeight.get_time path.total_weight)
end

(* ========================================================== *)
(* MAIN RUNNER                                                *)
(* ========================================================== *)

let () =
  let open Alcotest in
  run "MetroCaml Suite"
    [
      ( "eval_edge",
        [
          test_case "Marche sans horaires" `Quick EvalEdge_Tests.test_eval_edge_walk;
          test_case "Rester dans le train (Marge)" `Quick EvalEdge_Tests.test_eval_edge_same_line;
          test_case "Correspondance (Pénalité)" `Quick EvalEdge_Tests.test_eval_edge_transfer;
          test_case "Horaires épuisés" `Quick EvalEdge_Tests.test_eval_edge_missed_train;
          test_case "Perturbations Temps Réel" `Quick EvalEdge_Tests.test_eval_edge_perturbed_entry;
          test_case "Fermeture Temps Réel" `Quick EvalEdge_Tests.test_eval_edge_blocked;
        ] );
      ( "TimeWeight",
        [
          test_case "États de base et infini" `Quick test_timew_init_states;
          test_case "Traversée - Mode exclu" `Quick test_timew_traverse_excluded_mode;
          test_case "Traversée - Transport" `Quick test_timew_traverse_transport;
          test_case "Traversée - Cumul marche" `Quick test_timew_traverse_walk;
          test_case "Comparaison - Logique Infini" `Quick test_timew_compare_infinity;
          test_case "Comparaison - Temps dominant" `Quick test_timew_compare_time_dominates;
          test_case "Comparaison - Tie-Breaker Marche" `Quick test_timew_tie_breaker;
          test_case "Comparaison - Égalité parfaite" `Quick test_timew_perfect_tie;
          test_case "Formatage des données" `Quick test_timew_formatters;
        ] );
      ( "DistanceWeight",
        [
          test_case "États de base et infini" `Quick test_dw_init_states;
          test_case "Traversée - Mode exclu" `Quick test_dw_traverse_excluded_mode;
          test_case "Traversée - Accumulation" `Quick test_dw_traverse_accumulation;
          test_case "Comparaison - Logique Infini" `Quick test_dw_compare_infinity;
          test_case "Comparaison - Distance dominante" `Quick test_dw_compare_distance_dominates;
          test_case "Comparaison - Égalité parfaite" `Quick test_dw_compare_tie;
          test_case "Formatage intelligent (m/km)" `Quick test_dw_formatters;
        ] );
      ( "HopWeight",
        [
          test_case "États de base et infini" `Quick test_hopw_init_states;
          test_case "Traversée - Mode exclu" `Quick test_hopw_traverse_excluded_mode;
          test_case "Traversée - Incrémentation (+1)" `Quick test_hopw_traverse_accumulation;
          test_case "Comparaison - Logique Infini" `Quick test_hopw_compare_infinity;
          test_case "Comparaison - Sauts dominants" `Quick test_hopw_compare_hops_dominates;
          test_case "Comparaison - Égalité (Temps ignoré)" `Quick test_hopw_compare_tie;
          test_case "Formatage de l'affichage" `Quick test_hopw_formatters;
        ] );
      ( "CorrespondenceWeight",
        [
          test_case "États de base et infini" `Quick test_cw_init_states;
          test_case "Traversée - Mode exclu" `Quick test_cw_traverse_excluded_mode;
          test_case "Traversée - Rester sur la ligne (0)" `Quick test_cw_traverse_same_line;
          test_case "Traversée - Vraie correspondance (+1)" `Quick test_cw_traverse_real_transfer;
          test_case "Comparaison - Logique Infini" `Quick test_cw_compare_infinity;
          test_case "Comparaison - Confort dominant (Moins de correspondances)" `Quick test_cw_compare_transfers_dominates;
          test_case "Comparaison - Égalité (Temps ignoré)" `Quick test_cw_compare_tie;
        ] );
      ( "BFS",
        [
          test_case "Garde-fou : Noeud manquant" `Quick BFS_Tests.test_bfs_missing_node;
          test_case "Garde-fou : Source = Destination" `Quick BFS_Tests.test_bfs_same_node;
          test_case "Topologie : Chemin simple" `Quick BFS_Tests.test_bfs_simple_path;
          test_case "Topologie : Noeud inaccessible" `Quick BFS_Tests.test_bfs_unreachable;
          test_case "Topologie : BFS ignore les poids (Hops priority)" `Quick BFS_Tests.test_bfs_ignores_weights;
          test_case "Restriction : Arête bloquée (traverse=None)" `Quick BFS_Tests.test_bfs_excluded_mode;
          test_case "Intégration : Contournement de blocage" `Quick BFS_Tests.test_bfs_alternative_route_when_blocked;
          test_case "Intégration : Calcul des temps et Perturbations" `Quick BFS_Tests.test_bfs_perturbation_and_times;
        ] );
      ( "Dijkstra",
        [
          test_case "Topologie : Chemin linéaire simple" `Quick Dijkstra_Tests.test_dij_linear_path;
          test_case "Garde-fou : Noeuds manquants" `Quick Dijkstra_Tests.test_dij_missing_node;
          test_case "Garde-fou : Source = Destination" `Quick Dijkstra_Tests.test_dij_same_node;
          test_case "Topologie : Noeud inatteignable" `Quick Dijkstra_Tests.test_dij_unreachable_node;
          test_case "Optimalité : Choix du chemin le moins cher" `Quick Dijkstra_Tests.test_dij_optimal_path;
          test_case "State-Space : Anticipation des correspondances interdites" `Quick Dijkstra_Tests.test_dij_state_space_routing;
          test_case "Résilience : Changement d'itinéraire suite à perturbation" `Quick Dijkstra_Tests.test_dij_perturbation_impact;
          test_case "Résilience : Recalcul après blocage" `Quick Dijkstra_Tests.test_dij_rerouting_after_blockage;
        ] );
    ]