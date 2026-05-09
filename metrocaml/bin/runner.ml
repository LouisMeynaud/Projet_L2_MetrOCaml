open Metrocaml
open Weight
open Routing

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
  let transfer_allowed = if pmr_transfer then Util.is_pmr else fun _ -> true in
  List.map (fun (nom_algo, (module Routeur : ALGO)) ->
    let module ShowPath = Show.Make (Routeur.P) in
    let open Routeur.P in
    match Routeur.find_path ~start_time ~graph:graph_for_search ~source ~destination ~transfer_allowed with
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

(** Paramétrage des types *)

type stat = {
  total_time : int;
  transfers : int;
  arrival_time : int; 
  nb_steps : int;
}

type routeur =
  Network.TransportGraph.t -> Network.station -> Network.station -> int -> (stat option) * (unit -> unit)

(** Création d'un routeur avec son nom associé *)
let make_routeur nom (module Algo : ALGO) = 
  let compute_route graph source destination start_time =
    match Algo.find_path ~start_time ~graph ~source ~destination ~transfer_allowed:(fun _ -> true) with
    | None -> (None, fun () -> ())
    | Some p -> let module ShowPath = Show.Make (Algo.P) in
        let stats_show = ShowPath.compute_summary ~start_time p in
        let stats = { total_time = stats_show.total_time;  transfers = stats_show.transfers; arrival_time = stats_show.arrival_time; nb_steps = List.length p.Algo.P.steps } in

        (Some stats, fun () -> Printf.printf "%s\n" (ShowPath.string_of_path ~start_time ~use_color:true p))
  in
  (nom, compute_route)

(** Renvoie le bon type pour le benchmark générique unique *)
let print_bench_unique (nom, compute) = 

  let compute_route graph_for_search source destination start_time= 
    let result, (print_detail : unit -> unit) = compute graph_for_search source destination start_time in
    let result_str = match result with
      | None -> "Aucun chemin"
      | Some stats -> Printf.sprintf
          "OK - %d Etape(s)     %d Heures %d Minutes     Arrivé à %02d:%02d     en %d Correspondances"
          stats.nb_steps
          (stats.total_time / 3600) (stats.total_time / 60 mod 60)
          (stats.arrival_time / 3600 mod 24) (stats.arrival_time / 60 mod 60)
          stats.transfers
    in
    (result_str, print_detail)
  in 
  (nom, compute_route)
  

  (** Création des routeurs pour le benchmark unique *)
let build_bench_routeurs graph =
  let base_heuristic = make_heuristic graph in
  let preset_routeurs = List.concat_map (fun (preset : Presets.preset) ->
    let module W = (val Presets.to_weight preset) in
    let coef = preset.Presets.astar_coef in
    [
      print_bench_unique (make_routeur (Printf.sprintf "Dijkstra Preset : %s" preset.name) (make_dijkstra (module W)));
      print_bench_unique (make_routeur (Printf.sprintf "A* Preset : %s"      preset.name) (make_astar (module W) (fun a b -> base_heuristic a b *. coef)));
    ]
  ) Presets.all in
  let unit_routeurs = [
    print_bench_unique (make_routeur "Dijkstra : Temps"           (make_dijkstra (module Weight.TimeWeight)));
    print_bench_unique (make_routeur "Dijkstra : Distance"        (make_dijkstra (module Weight.DistanceWeight)));
    print_bench_unique (make_routeur "Dijkstra : Arrets"          (make_dijkstra (module Weight.HopWeight)));
    print_bench_unique (make_routeur "Dijkstra : Correspondances" (make_dijkstra (module Weight.CorrespondenceWeight)));
    print_bench_unique (make_routeur "A* : Temps"                 (make_astar (module Weight.TimeWeight)           base_heuristic));
    print_bench_unique (make_routeur "A* : Distance"              (make_astar (module Weight.DistanceWeight)       base_heuristic));
    print_bench_unique (make_routeur "A* : Arrets"                (make_astar (module Weight.HopWeight)            base_heuristic));
    print_bench_unique (make_routeur "A* : Correspondances"       (make_astar (module Weight.CorrespondenceWeight) base_heuristic));
    print_bench_unique (make_routeur "Arrets (BFS)"               (module BFS_HopWeight : ALGO));
  ] in
  preset_routeurs @ unit_routeurs

  

  (** Bench aléatoire*)

(** Fonctions pour les moyennes *)
let avg_elapsed l = 
  if List.length l = 0 then 0.0
  else List.fold_left (+.) 0.0 l /. float_of_int (List.length l)

let avg_weight l = 
  if List.length l = 0 then (0.0, 0.0)
  else let time_avg, transfers_avg = List.fold_left 
  (fun (time_acc, transfers_acc) w -> match w with
   |None -> time_acc, transfers_acc
   |Some stat -> (time_acc +. (float_of_int stat.total_time), transfers_acc +. (float_of_int stat.transfers))) 
   (0.0, 0.0) l in
  (time_avg /. float_of_int (List.length l), transfers_avg /. float_of_int (List.length l))

(** Fonctions pour exécuter les routeurs et calculer leurs bench/stats *)
let run_routeur (name, compute) graph source destination start_time =
  let t0 = Unix.gettimeofday () in
  let exception Timeout in
  let timeout_seconds = 30.0 in
  let old_handler = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
  try
    let _ = Unix.setitimer Unix.ITIMER_REAL {it_interval = 0.0; it_value = timeout_seconds} in 
    let stats, _ = compute graph source destination start_time in
    let _ = Unix.setitimer Unix.ITIMER_REAL {it_interval = 0.0; it_value = 0.0} in
    let elapsed = Unix.gettimeofday () -. t0 in
    let _ = Sys.signal Sys.sigalrm old_handler in
    (stats, elapsed)
  with Timeout ->
    let _ =  Unix.setitimer Unix.ITIMER_REAL {it_interval = 0.0; it_value = 0.0} in 
    let elapsed = Unix.gettimeofday () -. t0 in
    let _ = Sys.signal Sys.sigalrm old_handler in
    Printf.printf "Timeout dépassé pour %s (%.1f sec) - Aucun chemin trouvé\n%!" name elapsed;
    (None, elapsed)
  | e ->
    let _ = Unix.setitimer Unix.ITIMER_REAL {it_interval = 0.0; it_value = 0.0} in
    let _ = Sys.signal Sys.sigalrm old_handler in
    raise e

(** Validation d'une paire de routeurs en cherchant le chemin d'un nœud à un autre avec un algorithme donné *)
let validate_pair (module Algo : ALGO) graph source destination start_time =
  match
    Algo.find_path ~start_time ~graph ~source ~destination ~transfer_allowed:(fun _ -> true)
  with
  | None -> None
  | Some path -> Some (List.length path.Algo.P.steps)


(** Module pour utiliser le foncteur MakeRandomBench *)
module MetroCamlRandomBench : (Bench.Random_Bench 
  with type graph = Network.TransportGraph.t
   and type node = Network.station) = struct
  type graph = Network.TransportGraph.t
  type node = Network.station
  type routeur_pair = {
    preset_name : string;
    dijkstra : string * routeur;
    astar : string * routeur;
  }

(** Création de paires de routeurs A* et Dijkstra pour les bench aléatoires *)
  let make_routeur_pairs graph =
    let heuristic = make_heuristic graph in
    List.map
      (fun (preset : Presets.preset) ->
        let module W = (val Presets.to_weight preset) in
        {
          preset_name = preset.name;
          dijkstra =
            make_routeur (Printf.sprintf "Dijkstra Preset : %s" preset.name)
              (make_dijkstra (module W));
          astar =
            make_routeur (Printf.sprintf "A* Preset : %s" preset.name)
              (make_astar (module W) (fun a b -> heuristic a b *. preset.Presets.astar_coef));
        })
      Presets.all

  (** Récupération des nœuds d'un graphe *)
  let get_nodes graph = Network.TransportGraph.nodes graph

  (**  Trouver des points de départ et d'arrivée pour le benchmark valide*)
  let build_context graph src dst =
    match Endpoint.build_routing_context graph src.Network.id dst.Network.id with
    | None -> None
    | Some (g, s, d) -> Some (g, s, d)

  (** Lancer les benchmarks pour une paire de routeurs et calculer les statistiques *)
  let benchmark_pair routeur_pairs graph source destination start_time =
    match validate_pair (module BFS_HopWeight) graph source destination start_time with
    | None -> None
    | Some num_steps ->
        let category = Bench.category_of_steps num_steps in
        let dijkstra_tot_elapsed, astar_tot_elapsed, d_stats, a_stats =
          List.fold_left
            (fun (d_acc, a_acc, d_stats_acc, a_stats_acc) pair ->
              (* Printf.printf "Benchmarking %s\n%!" pair.preset_name; *)
              let d_stats, d_duration = run_routeur pair.dijkstra graph source destination start_time in
              (* Printf.printf "Dijkstra: %d steps, elapsed time: %.4f seconds\n%!" (match d_stats with Some s -> s.nb_steps | None -> -1) d_duration; *)
              let a_stats, a_duration = run_routeur pair.astar graph source destination start_time in
              (* Printf.printf "A*: %d steps, elapsed time: %.4f seconds\n%!" (match a_stats with Some s -> s.nb_steps | None -> -1) a_duration; *)
              (d_duration :: d_acc, a_duration :: a_acc, d_stats :: d_stats_acc, a_stats :: a_stats_acc))
            ([], [], [], []) routeur_pairs
        in
        let dijkstra_elapsed_avg = avg_elapsed dijkstra_tot_elapsed in
        let astar_elapsed_avg = avg_elapsed astar_tot_elapsed in
        let dijkstra_time, dijkstra_transfer = avg_weight d_stats in
        let astar_time, astar_transfer = avg_weight a_stats in
        let dijkstra_win_elapsed = if dijkstra_elapsed_avg < astar_elapsed_avg then 1 else 0 in
        let dijkstra_win_time = if dijkstra_time <= astar_time then 1 else 0 in
        let dijkstra_win_transfer = if dijkstra_transfer <= astar_transfer then 1 else 0 in
        let dijkstra_win_w = dijkstra_win_time + dijkstra_win_transfer in
        Some (category, dijkstra_win_elapsed, dijkstra_win_w, dijkstra_elapsed_avg, astar_elapsed_avg, dijkstra_transfer, astar_transfer)
end

module MetroCamlBench = Bench.MakeRandomBench(MetroCamlRandomBench)

let run_random_bench_console graph =
  MetroCamlBench.run_random_bench_console graph

let run_bench_unique graph =
  Printf.printf "\nBenchmark activé \n%!";
  let routeurs = build_bench_routeurs graph in
  Printf.printf "\nStation source : %!";
  let source_query = String.trim (read_line ()) in
  Printf.printf "Station destination : %!";
  let dest_query = String.trim (read_line ()) in
  let tm = Unix.localtime (Unix.time ()) in
  let start_time = tm.Unix.tm_hour * 3600 + tm.Unix.tm_min * 60 in
  Routing.Bench.run
    ~title:"Benchmark MetroCaml"
    ~resolve:(fun g src dst -> Endpoint.build_routing_context g src dst)
    graph source_query dest_query start_time routeurs Cli.Console.raw_select

let rec run_bench_console graph =
  Printf.printf "\n=== Mode Benchmark ===\n%!";
  let mode = Cli.Console.raw_select "Quel mode de benchmark voulez-vous ?" [ "Unique"; "Aleatoire"] in
  let _  = 
  if mode = "Unique" then
    run_bench_unique graph
  else
    run_random_bench_console graph 
  in
  let continue = Cli.Console.raw_select "\nVoulez-vous effectuer un nouveau benchmark ? " [ "Oui"; "Non" ] in
  if continue = "Oui" then run_bench_console graph
  else Printf.printf "Fermeture du mode benchmark.\n%!"
  
