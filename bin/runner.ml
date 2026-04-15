open Metrocaml
open Weight
open Routing

module DIJKSTRA_CorrespondenceWeight = Dijkstra.Make (Network.TransportGraph) (Weight.CorrespondenceWeight)
module DIJKSTRA_DistanceWeight =       Dijkstra.Make (Network.TransportGraph) (Weight.DistanceWeight)
module DIJKSTRA_TimeWeight =           Dijkstra.Make (Network.TransportGraph) (Weight.TimeWeight)
module DIJKSTRA_HopWeight =            Dijkstra.Make (Network.TransportGraph) (Weight.HopWeight)
module BFS_HopWeight =                 Bfs.Make (Network.TransportGraph) (Weight.HopWeight)

module type ALGO = sig
  module P :
    Routing.PATH
      with type Node.t = Network.station
       and type Edge.t = Network.connection

  val find_path :
    start_time:int ->
    graph:Network.TransportGraph.t ->
    source:Network.station ->
    destination:Network.station ->
    transfer_allowed:(Network.station -> bool) ->
    P.t option

end

(* Fonction pour charger les données GTFS *)
let load_gtfs_data town url date_cible loading_mode =
  let local_data_dir = "data/" ^ town in
  try
    let graphe =
      if loading_mode = "Local" then begin
        Printf.printf "Chargement local GTFS depuis %s...\n%!" local_data_dir;
        Gtfs_adapter.load_network_gtfs local_data_dir date_cible
      end
      else begin
        Printf.printf "Téléchargement en cours...\n%!";
        Gtfs_adapter.load_network_from_url url date_cible
      end
    in
    flush stdout;
    graphe
  with
  | Failure msg ->
      Printf.eprintf "ERREUR : %s\n" msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "ERREUR FICHIER : %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "ERREUR INCONNUE : %s\n" (Printexc.to_string e);
      exit 1

(* Fonction pour lancer la recherche de trajet *)

let choose_weight poids coeffs pmr_transfer =
  match poids with
  | "Default" ->
      List.map
        (fun (preset : Presets.preset) ->
          let module W = (val Presets.to_weight preset) in
          let module D = Dijkstra.Make (Network.TransportGraph) (W) in
          (preset.name, (module D : ALGO)))
        Presets.all
  | "Combinaison" ->
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
            : Routing.WEIGHT with type edge = Network.connection)
      in
      let module DIJKSTRA_CombinedWeight =
        Dijkstra.Make (Network.TransportGraph) (CombinedWeight) in
      [ ("Combinaison", (module DIJKSTRA_CombinedWeight)) ]
  | _ -> 
          [ ("Temps", (module DIJKSTRA_TimeWeight)) ]

let affichage details_mode use_color start_time source destination
    graph_for_search routeurs pmr_transfer =
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
            let details =
              ShowPath.string_of_path
                (fun s -> s.name)
                (fun (c : Network.connection) -> c.line.name)
                Routeur.P.Weight.to_string Routeur.P.Weight.get_time
                (fun (c : Network.connection) -> c.line.color)
                (fun (c : Network.connection) -> c.duration)
                (fun (c : Network.connection) -> c.distance)
                start_time details_mode use_color p
            in
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
    with Not_found -> Printf.printf "Algorithme non trouvé.\n%!"

let search_callback source_query dest_query algo poids coeffs excluded_modes
    start_time pmr_transfer graphe details_mode use_color =
  match Endpoint.build_routing_context graphe source_query dest_query with
  | Some (graph_for_search, source, destination) ->
      Weight.set_excluded_modes excluded_modes;
      let routeurs : (string * (module ALGO)) list =
        if algo = "BFS" then
          [
            ( "BFS",
              (module struct
                module P = BFS_HopWeight.P

                let find_path ~start_time ~graph ~source ~destination ~transfer_allowed:_ =
                  BFS_HopWeight.find_path ~start_time ~graph ~source ~destination
              end) );
          ]
        else choose_weight poids coeffs pmr_transfer
      in
      let details =
        affichage details_mode use_color start_time source destination
          graph_for_search routeurs pmr_transfer
      in
      details
  | None ->
      [
        ( "Erreur : Stations introuvables, coordonnées invalides ou adresse introuvable.",
          "",
          None );
      ]
