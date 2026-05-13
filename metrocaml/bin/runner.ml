open Metrocaml
open Weight
open Routing
open Metrobench


module BFS_HopWeight = Bfs.Make (Network.TransportGraph) (Weight.HopWeight)

module type ALGO = Routing.ALGO
  with type graph = Network.TransportGraph.t
  with type P.Node.t = Network.station
  with type P.Edge.t = Network.connection

let make_dijkstra
    (module W : Routing.WEIGHT with type edge = Network.connection and type node = Network.station) =
  let module D = Dijkstra.Make (Network.TransportGraph) (W) in
  (module D : ALGO)

let make_astar
    (module W : Routing.WEIGHT with type edge = Network.connection and type node = Network.station)
    heuristic_fn =
  let module H = struct type node = Network.station let estimate = heuristic_fn end in
  (module Astar.Make (Network.TransportGraph) (W) (H) : ALGO)

(** Construit l'heuristique A* pour un réseau GTFS donné.
    Parcourt toutes les arêtes véhiculées pour extraire la vitesse maximale observée sur le réseau.
    Retourne une fonction station -> station -> float estimant le temps de trajet minimal
    restant (haversine / v_max). Repli sur 13.89 m/s (~50 km/h) si le graphe est vide. *)
let make_heuristic (graph : Network.TransportGraph.t) =
  let v_max =
    List.fold_left (fun acc node ->
      List.fold_left (fun acc' (_, (conn : Network.connection)) ->
        if not (Util.edge_is_walk conn) && conn.duration > 0 && conn.distance > 0.0 then
          Float.max acc' (conn.distance /. float_of_int conn.duration)
        else acc'
      ) acc (Network.TransportGraph.neighbors graph node)
    ) 0.0 (Network.TransportGraph.nodes graph)
  in
  let v_max = if v_max <= 0.0 then 13.89 else v_max in
  fun (a : Network.station) (b : Network.station) ->
    Util.haversine_distance a.latitude a.longitude b.latitude b.longitude /. v_max

(* Chargement des données GTFS *)
let load_gtfs_data town url date_cible loading_mode =
  let local_data_dir =
    let p1 = "data/" ^ town in
    let p2 = "metrocaml/data/" ^ town in
    if Sys.file_exists p1 then p1 else p2
  in
  try
    let graphe =
      if loading_mode = "Local" then begin
        Logger.black "Chargement local GTFS depuis %s...\n%!" local_data_dir;
        Gtfs_adapter.load_network_gtfs local_data_dir date_cible
      end else begin
        Logger.black "Téléchargement en cours...\n%!";
        Gtfs_adapter.load_network_from_url url date_cible
      end
    in
    flush stdout;
    graphe
  with
  | Failure msg   -> Logger.bold_red "ERREUR : %s\n" msg;         exit 1
  | Sys_error msg -> Logger.bold_red "ERREUR FICHIER : %s\n" msg; exit 1
  | e             -> Logger.bold_red "ERREUR INCONNUE : %s\n" (Printexc.to_string e); exit 1

let unpack_coeffs = function
  | [ct; cd; ca; cc] -> (ct, cd, ca, cc)
  | _                -> (1.0, 1.0, 1.0, 1.0)

let build_combined_weight (ct, cd, ca, cc) =
  Combinator.combine [
    Combinator.criterion (module Weight.TimeWeight)           ~coefficient:ct ~label:"Temps";
    Combinator.criterion (module Weight.DistanceWeight)       ~coefficient:cd ~label:"Distance";
    Combinator.criterion (module Weight.HopWeight)            ~coefficient:ca ~label:"Arrêts";
    Combinator.criterion (module Weight.CorrespondenceWeight) ~coefficient:cc ~label:"Correspondances";
  ]

let choose_weight algo poids coeffs graph =
  match algo, poids with
  | "Dijkstra", "Default" ->
      List.map (fun (preset : Presets.preset) ->
        let module W = (val Presets.to_weight preset) in
        (preset.name, make_dijkstra (module W))
      ) Presets.all
  | "Dijkstra", "Combinaison" ->
      let module CW = (val build_combined_weight (unpack_coeffs coeffs)
        : Routing.WEIGHT with type edge = Network.connection and type node = Network.station) in
      [ ("Combinaison", make_dijkstra (module CW)) ]
  | "A*", "Default" ->
      let base = make_heuristic graph in
      List.map (fun (preset : Presets.preset) ->
        let module W = (val Presets.to_weight preset) in
        let coef = preset.Presets.astar_coef in
        (preset.name, make_astar (module W) (fun a b -> base a b *. coef))
      ) Presets.all
  | "A*", "Combinaison" ->
      let (ct, _, _, _) as coeffs_tuple = unpack_coeffs coeffs in
      let module CW = (val build_combined_weight coeffs_tuple
        : Routing.WEIGHT with type edge = Network.connection and type node = Network.station) in
      let base = make_heuristic graph in
      [ ("Combinaison A*", make_astar (module CW) (fun a b -> base a b *. ct)) ]
  | _ ->
      [ ("Temps", make_dijkstra (module Weight.TimeWeight)) ]

let affichage use_color start_time source destination graph_for_search routeurs pmr_transfer =
  let can_change_state = if pmr_transfer then Util.is_pmr else fun _ -> true in
  List.map (fun (nom_algo, (module Routeur : ALGO)) ->
    let module ShowPath = Show.Make (Routeur.P) in
    let open Routeur.P in
    match Routeur.find_path ~init:(Routeur.P.Weight.start_at start_time) ~graph:graph_for_search ~source ~destination ~can_change_state with
    | Some p ->
        let edges = List.map (fun step -> (step.from.id, step.to_node.id, step.edge.line.name)) p.steps in
        (nom_algo, ShowPath.string_of_path ~start_time ~use_color p, Some edges)
    | None ->
        (nom_algo, "Aucun chemin trouvé.", None)
  ) routeurs

let search_callback source_query dest_query algo poids coeffs excluded_modes
    start_time pmr_transfer graphe use_color =
  match Endpoint.build_routing_context graphe source_query dest_query with
  | Some (graph_for_search, source, destination) ->
      Weight.set_excluded_modes excluded_modes;
      let routeurs =
        if algo = "BFS" then [ ("BFS", (module BFS_HopWeight : ALGO)) ]
        else choose_weight algo poids coeffs graph_for_search
      in
      affichage use_color start_time source destination graph_for_search routeurs pmr_transfer
  | None ->
      [ ("\x1b[31mErreur : Stations introuvables, coordonnées invalides ou adresse introuvable.\x1b[0m", "", None) ]


(** Benchmarking *)

(** Création des routeurs pour le benchmark unique *)
let build_bench_routeurs graph =
  let base_heuristic = make_heuristic graph in
  let preset_routeurs = List.concat_map (fun (preset : Presets.preset) ->
    let module W = (val Presets.to_weight preset) in
    let coef = preset.Presets.astar_coef in
    [
      bench_routeur_unique (Printf.sprintf "Dijkstra Preset : %s" preset.name) (make_dijkstra (module W));
      bench_routeur_unique (Printf.sprintf "A* Preset : %s"      preset.name) (make_astar (module W) (fun a b -> base_heuristic a b *. coef));
    ]
  ) Presets.all in
  let unit_routeurs = [
    bench_routeur_unique "Dijkstra : Temps"           (make_dijkstra (module Weight.TimeWeight));
    bench_routeur_unique "Dijkstra : Distance"        (make_dijkstra (module Weight.DistanceWeight));
    bench_routeur_unique "Dijkstra : Arrets"          (make_dijkstra (module Weight.HopWeight));
    bench_routeur_unique "Dijkstra : Correspondances" (make_dijkstra (module Weight.CorrespondenceWeight));
    bench_routeur_unique "A* : Temps"                 (make_astar (module Weight.TimeWeight)           base_heuristic);
    bench_routeur_unique "A* : Distance"              (make_astar (module Weight.DistanceWeight)       base_heuristic);
    bench_routeur_unique "A* : Arrets"                (make_astar (module Weight.HopWeight)            base_heuristic);
    bench_routeur_unique "A* : Correspondances"       (make_astar (module Weight.CorrespondenceWeight) base_heuristic);
    bench_routeur_unique "Arrets (BFS)"               (module BFS_HopWeight : ALGO);
  ] in
  preset_routeurs @ unit_routeurs


(** Création de paires de routeurs A* et Dijkstra pour les bench aléatoires *)



let make_routeur_pairs graph : MetroCamlRandomBench.routeur_pair list =
  let heuristic = make_heuristic graph in
    List.map
      (fun (preset : Presets.preset) ->
        let module W = (val Presets.to_weight preset) in
        MetroCamlRandomBench.{
          preset_name = preset.name;
          dijkstra =
            make_routeur (Printf.sprintf "Dijkstra Preset : %s" preset.name)
              (make_dijkstra (module W));
          astar =
            make_routeur (Printf.sprintf "A* Preset : %s" preset.name)
              (make_astar (module W) (fun a b -> heuristic a b *. preset.Presets.astar_coef));
        })
      Presets.all
  

module Routeurs : CreateRouteurPairs = struct let make_routeur_pairs = make_routeur_pairs end

let rec run_bench_console graph =
  Printf.printf "\n=== Mode Benchmark ===\n%!";
  let mode = Cli.Console.raw_select "Quel mode de benchmark voulez-vous ?" [ "Unique"; "Aleatoire"] in
  let _  = 
  if mode = "Unique" then
    run_bench_unique graph (build_bench_routeurs graph)
  else
    run_random_bench_console (module Routeurs) graph
  in
  let continue = Cli.Console.raw_select "\nVoulez-vous effectuer un nouveau benchmark ? " [ "Oui"; "Non" ] in
  if continue = "Oui" then run_bench_console graph
  else Printf.printf "Fermeture du mode benchmark.\n%!"
  
