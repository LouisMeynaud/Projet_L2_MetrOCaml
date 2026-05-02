open Metrocaml
open Weight
open Routing

module DIJKSTRA_CorrespondenceWeight = Dijkstra.Make (Network.TransportGraph) (Weight.CorrespondenceWeight)
module DIJKSTRA_DistanceWeight =       Dijkstra.Make (Network.TransportGraph) (Weight.DistanceWeight)
module DIJKSTRA_TimeWeight =           Dijkstra.Make (Network.TransportGraph) (Weight.TimeWeight)
module DIJKSTRA_HopWeight =            Dijkstra.Make (Network.TransportGraph) (Weight.HopWeight)
module BFS_HopWeight =                 Bfs.Make (Network.TransportGraph) (Weight.HopWeight)
module ASTAR_CorrespondenceWeight =    Astar.Make (Network.TransportGraph) (Weight.CorrespondenceWeight)
module ASTAR_DistanceWeight =          Astar.Make (Network.TransportGraph) (Weight.DistanceWeight)
module ASTAR_TimeWeight =              Astar.Make (Network.TransportGraph) (Weight.TimeWeight)
module ASTAR_HopWeight =               Astar.Make (Network.TransportGraph) (Weight.HopWeight)

module type ALGO = Routing.ALGO
  with type graph = Network.TransportGraph.t
  with type P.Node.t = Network.station
  with type P.Edge.t = Network.connection

(** Construit l'heuristique A* pour un réseau GTFS donné.
    Parcourt toutes les arêtes véhiculées pour extraire la vitesse maximale observée sur le réseau.
    Retourne une fonction de type station -> station -> float estimant le temps de trajet minimal
    restant entre deux stations (haversine / v_max).
    En cas de graphe vide ou sans connexion véhiculée valide, v_max est remplacé par 13.89 m/s (~50 km/h).
    Le coefficient appliqué pour les presets Rapide (0.75) et Confort (0.5) correspond à la part du
    critère Temps dans ces presets.*)
let make_heuristic (graph : Network.TransportGraph.t) =
  let v_max =
    List.fold_left (fun acc_v node ->
      List.fold_left (fun acc_v' (_, (conn : Network.connection)) ->
        if not (Util.edge_is_walk conn) && conn.duration > 0 && conn.distance > 0.0 then
          Float.max acc_v' (conn.distance /. float_of_int conn.duration)
        else acc_v'
      ) acc_v (Network.TransportGraph.neighbors graph node)
    ) 0.0 (Network.TransportGraph.nodes graph)
  in
  let v_max = if v_max <= 0.0 then 13.89 else v_max in
  fun (a : Network.station) (b : Network.station) ->
    Util.haversine_distance a.latitude a.longitude b.latitude b.longitude /. v_max

(* Fonction pour charger les données GTFS *)
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
      end
      else begin
        Logger.black "Téléchargement en cours...\n%!";
        Gtfs_adapter.load_network_from_url url date_cible
      end
    in
    flush stdout;
    graphe
  with
  | Failure msg ->
      Logger.bold_red "ERREUR : %s\n" msg;
      exit 1
  | Sys_error msg ->
      Logger.bold_red "ERREUR FICHIER : %s\n" msg;
      exit 1
  | e ->
      Logger.bold_red "ERREUR INCONNUE : %s\n" (Printexc.to_string e);
      exit 1

(* Fonction pour lancer la recherche de trajet *)
let choose_weight algo poids coeffs pmr_transfer graph =
  match algo, poids with
  | "Dijkstra", "Default" ->
      List.map
        (fun (preset : Presets.preset) ->
          let module W = (val Presets.to_weight preset) in
          let module D = Dijkstra.Make (Network.TransportGraph) (W) in
          (preset.name, (module D : ALGO)))
        Presets.all
  | "Dijkstra", "Combinaison" ->
      (* Création dynamique du module CombinedWeight avec les coefficients récupérés des Sliders *)
      let ct, cd, ca, cc =
        match coeffs with
        | [ a; b; c; d ] -> (a, b, c, d)
        | _ -> (1.0, 1.0, 1.0, 1.0)
      in
      let critere =
        [
          Combinator.criterion (module Weight.TimeWeight) ~coefficient:ct ~label:"Temps";
          Combinator.criterion (module Weight.DistanceWeight) ~coefficient:cd ~label:"Distance";
          Combinator.criterion (module Weight.HopWeight) ~coefficient:ca ~label:"Arrêts";
          Combinator.criterion (module Weight.CorrespondenceWeight) ~coefficient:cc ~label:"Correspondances";
        ]
      in
      let module CombinedWeight =
        (val Combinator.combine critere
            : Routing.WEIGHT with type edge = Network.connection and type node = Network.station)
      in
      let module DIJKSTRA_CombinedWeight =
      Dijkstra.Make (Network.TransportGraph) (CombinedWeight) in
      [ ("Combinaison", (module DIJKSTRA_CombinedWeight)) ]
  | "A*", "Default" ->
      List.map
        (fun (preset : Presets.preset) ->
          let module W = (val Presets.to_weight preset) in
          let module A = Astar.Make (Network.TransportGraph) (W) in
          let base = make_heuristic graph in
          let coef =
            match preset.name with
            | "Rapide" -> 0.75
            | "Confort" -> 0.5
            | _ -> 1.0
          in
          let heuristic a b = base a b *. coef in
          (preset.name, (module struct
            module P = A.P
            type graph = Network.TransportGraph.t
            let find_path ~start_time ~graph ~source ~destination ~transfer_allowed =
              A.find_path ~start_time ~graph ~source ~destination ~transfer_allowed ~heuristic
          end : ALGO)))
          (* can't use A directly because it doesn't match the signature. need to redefine find_path = A's find_path with the heuristic. this does not work if type "graph" is not provided.*)
        Presets.all
  | "A*", "Combinaison" ->
      let ct, cd, ca, cc =
        match coeffs with
        | [ a; b; c; d ] -> (a, b, c, d)
        | _ -> (1.0, 1.0, 1.0, 1.0)
      in
      let critere =
        [
          Combinator.criterion (module Weight.TimeWeight)           ~coefficient:ct ~label:"Temps";
          Combinator.criterion (module Weight.DistanceWeight)       ~coefficient:cd ~label:"Distance";
          Combinator.criterion (module Weight.HopWeight)            ~coefficient:ca ~label:"Arrêts";
          Combinator.criterion (module Weight.CorrespondenceWeight) ~coefficient:cc ~label:"Correspondances";
        ]
      in
      let module CombinedWeight =
        (val Combinator.combine critere
            : Routing.WEIGHT with type edge = Network.connection and type node = Network.station)
      in
      let module A = Astar.Make (Network.TransportGraph) (CombinedWeight) in
      let base = make_heuristic graph in
      let heuristic a b = base a b *. ct in
      [ ("Combinaison A*",
         (module struct
           module P = A.P
            type graph = Network.TransportGraph.t
           let find_path ~start_time ~graph ~source ~destination ~transfer_allowed =
             A.find_path ~start_time ~graph ~source ~destination
               ~transfer_allowed ~heuristic
         end : ALGO)) ]
  | _ ->
      [ ("Temps", (module DIJKSTRA_TimeWeight : ALGO)) ]

let affichage use_color start_time source destination graph_for_search routeurs pmr_transfer =
  (* Accumulation fonctionnelle des détails d'affichage *)
  let results =
    List.fold_left
      (fun acc (nom_algo, (module Routeur : ALGO)) ->
        let module ShowPath = Show.Make (Routeur.P) in
        let open Routeur.P in
        match
          Routeur.find_path ~start_time ~graph:graph_for_search ~source
            ~destination ~transfer_allowed:(if pmr_transfer then Util.is_pmr else fun _ -> true)
        with
        | Some p ->
            let details = ShowPath.string_of_path ~start_time ~use_color p in
            let edges =
              List.map (fun step -> (step.from.id, step.to_node.id, step.edge.line.name)) p.steps
            in
            acc @ [ (nom_algo, details, Some edges) ]
        | None -> acc @ [ (nom_algo, "Aucun chemin trouvé.", None) ])
      [] routeurs
  in
  results

let show_details details_afficheurs =
(* Proposer à l'utilisateur de voir les détails d'un chemin spécifique *)
  if details_afficheurs <> [] then
    let noms_algos = List.map fst details_afficheurs in
    let choix =
      Cli.Console.raw_select
        "\nVoir les détails du chemin avec quel algorithme ?" noms_algos
    in
    try
      let _, afficher_detail =
        List.find (fun (nom, _) -> nom = choix) details_afficheurs
      in
      afficher_detail ();
      flush stdout
    with Not_found -> Logger.bold_red "Algorithme non trouvé.\n%!"

let search_callback source_query dest_query algo poids coeffs excluded_modes
  start_time pmr_transfer graphe use_color =
  match Endpoint.build_routing_context graphe source_query dest_query with
  | Some (graph_for_search, source, destination) ->
      Weight.set_excluded_modes excluded_modes;
      let routeurs : (string * (module ALGO)) list =
        if algo = "BFS" then
          [
            ( "BFS",
              (module struct
                module P = BFS_HopWeight.P
                type graph = Network.TransportGraph.t

                let find_path ~start_time ~graph ~source ~destination ~transfer_allowed:_ =
                  BFS_HopWeight.find_path ~start_time ~graph ~source ~destination
              end) );
          ]
        else choose_weight algo poids coeffs pmr_transfer graph_for_search
      in
      let details = affichage use_color start_time source destination graph_for_search routeurs pmr_transfer in
      details
  | None ->
      [
        ( "\x1b[31mErreur : Stations introuvables, coordonnées invalides ou adresse introuvable.\x1b[0m",
          "",
          None );
      ]

(* Construit un routeur pour les tests de bench *)
let make_routeur nom (module Algo : ALGO) =
  let compute_route graph source destination start_time =
    match
      Algo.find_path ~start_time ~graph ~source ~destination
        ~transfer_allowed:(fun _ -> true)
    with
    | None -> ("Aucun chemin", fun () -> ())
    | Some p ->
        let module ShowPath = Show.Make (Algo.P) in
        let stats = ShowPath.compute_summary ~start_time p in
        let temps = stats.total_time in
        let correspondance = stats.transfers in 
        let arrival_time = stats.arrival_time in 
        let h = temps / 3600 in
        let m = temps / 60 mod 60 in
        let h_arr = arrival_time / 3600 mod 24 in
        let m_arr = arrival_time / 60 mod 60 in
        let result_str = Printf.sprintf "OK - %d Etape(s)     %d Heures %d Minutes     Arrivé à %d:%d     en %d Correspondances" (List.length p.Algo.P.steps) h m h_arr m_arr correspondance in
        let print () = Printf.printf "%s\n" (ShowPath.string_of_path ~start_time ~use_color:true p) in
        (result_str, print)
  in
  (nom, compute_route)



let build_bench_routeurs graph =
  let base_heuristic = make_heuristic graph in
  let preset_dijkstra =
    List.map (fun (preset : Presets.preset) ->
      let module W = (val Presets.to_weight preset) in
      let module D = Dijkstra.Make (Network.TransportGraph) (W) in
      make_routeur
        (Printf.sprintf "Dijkstra Preset : %s" preset.name)
        (module struct
          module P = D.P
          type graph = Network.TransportGraph.t
          let find_path = D.find_path
        end)
    ) Presets.all
  in
  let preset_astar =
    List.map (fun (preset : Presets.preset) ->
      let module W = (val Presets.to_weight preset) in
      let module A = Astar.Make (Network.TransportGraph) (W) in
      let coef =
            match preset.name with
            | "Rapide" -> 0.75
            | "Confort" -> 0.5
            | _ -> 1.0
          in
          let heuristic a b = base_heuristic a b *. coef in
      make_routeur
        (Printf.sprintf "A* Preset : %s" preset.name)
        (module struct
          module P = A.P
          type graph = Network.TransportGraph.t
          let find_path ~start_time ~graph ~source ~destination ~transfer_allowed =
            A.find_path ~start_time ~graph ~source ~destination
              ~transfer_allowed ~heuristic
        end)
    ) Presets.all
  in
  let unit_routeurs = [
    make_routeur "Dijkstra : Temps"
      (module struct
        module P = DIJKSTRA_TimeWeight.P
        type graph = Network.TransportGraph.t
        let find_path = DIJKSTRA_TimeWeight.find_path
      end);
    make_routeur "Dijkstra : Distance"
      (module struct
        module P = DIJKSTRA_DistanceWeight.P
        type graph = Network.TransportGraph.t
        let find_path = DIJKSTRA_DistanceWeight.find_path
      end);
    make_routeur "Dijkstra : Arrets"
      (module struct
        module P = DIJKSTRA_HopWeight.P
        type graph = Network.TransportGraph.t
        let find_path = DIJKSTRA_HopWeight.find_path
      end);
    make_routeur "Dijkstra : Correspondances"
      (module struct
        module P = DIJKSTRA_CorrespondenceWeight.P
        type graph = Network.TransportGraph.t
        let find_path = DIJKSTRA_CorrespondenceWeight.find_path
      end);
    make_routeur "A* : Temps"
      (module struct
        module P = ASTAR_TimeWeight.P
        type graph = Network.TransportGraph.t
        let find_path ~start_time ~graph ~source ~destination ~transfer_allowed =
          ASTAR_TimeWeight.find_path ~start_time ~graph ~source ~destination
            ~transfer_allowed ~heuristic:base_heuristic
      end);
    make_routeur "A* : Distance"
      (module struct
        module P = ASTAR_DistanceWeight.P
        type graph = Network.TransportGraph.t
        let find_path ~start_time ~graph ~source ~destination ~transfer_allowed =
          ASTAR_DistanceWeight.find_path ~start_time ~graph ~source ~destination
            ~transfer_allowed ~heuristic:base_heuristic
      end);
    make_routeur "A* : Arrets"
      (module struct
        module P = ASTAR_HopWeight.P
        type graph = Network.TransportGraph.t
        let find_path ~start_time ~graph ~source ~destination ~transfer_allowed =
          ASTAR_HopWeight.find_path ~start_time ~graph ~source ~destination
            ~transfer_allowed ~heuristic:base_heuristic
      end);
    make_routeur "A* : Correspondances"
      (module struct
        module P = ASTAR_CorrespondenceWeight.P
        type graph = Network.TransportGraph.t
        let find_path ~start_time ~graph ~source ~destination ~transfer_allowed =
          ASTAR_CorrespondenceWeight.find_path ~start_time ~graph ~source ~destination
            ~transfer_allowed ~heuristic:base_heuristic
      end);
    make_routeur "Arrets (BFS) : "
      (module struct
        module P = BFS_HopWeight.P
        type graph = Network.TransportGraph.t
        let find_path ~start_time ~graph ~source ~destination ~transfer_allowed:_ =
          BFS_HopWeight.find_path ~start_time ~graph ~source ~destination
      end);
  ] in
  preset_dijkstra @ preset_astar @ unit_routeurs

  let rec run_bench_console graphe =
  Printf.printf "\nBenchmark activé \n%!";
  let routeurs = build_bench_routeurs graphe in
    Printf.printf "\nStation source : %!";
    let source_query = String.trim (read_line ()) in
    Printf.printf "Station destination : %!";
    let dest_query = String.trim (read_line ()) in
    let tm = Unix.localtime (Unix.time ()) in
    let start_time = tm.Unix.tm_hour * 3600 + tm.Unix.tm_min * 60 in
    Routing.Bench.run
      ~title:"Benchmark"
      ~resolve:(fun g src dst -> Endpoint.build_routing_context g src dst)
      graphe source_query dest_query start_time routeurs Cli.Console.raw_select;
    let continue = Cli.Console.raw_select "\nVoulez-vous effectuer un nouveau benchmark ? " [ "Oui"; "Non" ] in
    if continue = "Oui" then
      run_bench_console graphe
    else
      Printf.printf "Fermeture du mode benchmark.\n%!";
