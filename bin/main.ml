open Metrocaml
open Weight
open Routing

module DIJKSTRA_CorrespondenceWeight = Dijkstra.Make(Network.TransportGraph)(Weight.CorrespondenceWeight)
module DIJKSTRA_DistanceWeight       = Dijkstra.Make(Network.TransportGraph)(Weight.DistanceWeight)
module DIJKSTRA_TimeWeight           = Dijkstra.Make(Network.TransportGraph)(Weight.TimeWeight)
module DIJKSTRA_HopWeight            = Dijkstra.Make(Network.TransportGraph)(Weight.HopWeight)
module BFS_HopWeight                 = Bfs.Make(Network.TransportGraph)(Weight.HopWeight)

module type ALGO = sig
  module P : Routing.PATH with type Node.t = Network.station and type Edge.t = Network.connection

  val find_path :
    start_time:int ->
    graph:Network.TransportGraph.t ->
    source:Network.station ->
    destination:Network.station ->
    transfer_allowed:(Network.station -> bool) ->
    P.t option
end

let today_yyyymmdd () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

let () =

  let loading_mode = Cli.Console.raw_select "\nChoisissez le mode de chargement : " [ "En ligne"; "Local"] in
  let town = match loading_mode with
    | "En ligne" -> Cli.Console.raw_select "Choisissez la ville : " [ "Toulouse"; "Montpellier"; "Marseille"; "Ile-de-France"; "SNCF"; "Corsica"]
    | "Local" -> 
      Cli.Console.raw_select "Choisissez la ville : "
      (List.filter(fun ville -> try (Unix.stat ("data/" ^ ville )).Unix.st_kind = Unix.S_DIR (*Vérifie si le dossier existe et si c'est bien un dossier (S_DIR)*)
                                with _ -> false (*Plutôt que de planter on retourne false.*))
      [ "Toulouse"; "Montpellier"; "Marseille"; "Ile-de-France"; "SNCF"; "Corsica"])
    | _ -> failwith "Mode de chargement inconnu"
  in

  let url =
    match town with
    | "Toulouse" -> "https://www.data.gouv.fr/api/1/datasets/r/64318177-f5b5-4144-a6be-2d0f22d26c77"
    | "Montpellier" -> "https://data.montpellier3m.fr/sites/default/files/ressources/TAM_MMM_GTFS.zip"
    | "Marseille" -> "https://www.data.gouv.fr/api/1/datasets/r/ab931edc-0bc1-4c66-960a-dc546b46df03"
    | "Ile-de-France" -> "https://eu.ftp.opendatasoft.com/stif/GTFS/IDFM-gtfs.zip"
    | "SNCF" -> "https://www.data.gouv.fr/api/1/datasets/r/9ae758ec-cd7a-40cd-a890-bb3963224942"
    | "Corsica" -> "https://www.data.gouv.fr/api/1/datasets/r/b95d6838-1143-4653-840f-eebc33c16fa7"
    | _ -> failwith "Ville inconnue"
  in


  let date_cible = 
    match loading_mode with
    | "En ligne" -> today_yyyymmdd ()
    | "Local" -> "20260330"
    | _ -> failwith "Mode de chargement inconnu"
  in
  Printf.printf "\n=== Lancement de MetroCaml ===\n%!";
  Printf.printf "Date GTFS cible : %s\n%!" date_cible;
  
  let (rt_url_alertes) = 
    match town with
    | "Ile-de-France" -> 
        ( "https://prim.iledefrance-mobilites.fr/marketplace/disruptions_bulk/disruptions/v2")
    | _ -> ("") 
  in
  if (rt_url_alertes <> "" && loading_mode == "En ligne" ) then begin
    Printf.printf "\n[Système] Démarrage du moteur Temps Réel en arrière-plan...\n%!";
    Gtfs_rt.start_auto_refresh date_cible ;
  end;

  let local_data_dir =  "data/" ^ town in

  try
    let graphe =
      if loading_mode == "Local" then begin
        Printf.printf "Chargement local GTFS depuis %s...\n%!" local_data_dir;
        Gtfs_adapter.load_network_gtfs local_data_dir date_cible
      end else begin
        Printf.printf "Téléchargement en cours...\n%!";
        Gtfs_adapter.load_network_from_url url date_cible
      end
    in
    flush stdout;
    
    let search_callback source_query dest_query algo poids coeffs excluded_modes start_time pmr_transfer details_mode =
      match Endpoint.build_routing_context graphe source_query dest_query with
      | Some (graph_for_search, source, destination) ->
          Weight.set_excluded_modes excluded_modes;
          let routeurs : (string * (module ALGO)) list =
            if algo = "BFS" then
              [("BFS", (module struct
                module P = BFS_HopWeight.P
                let find_path ~start_time ~graph ~source ~destination ~transfer_allowed:_ =
                  BFS_HopWeight.find_path ~start_time ~graph ~source ~destination
              end))]
            else
              match poids with
                | "Default" -> List.map (fun (preset : Presets.preset) -> 
                    let module W = (val Presets.to_weight preset) in
                    let module D = Dijkstra.Make(Network.TransportGraph)(W) in
                    (preset.name, (module D : ALGO))
                  ) Presets.all
                | "Combinaison"     -> 
                      (* Création dynamique du module CombinedWeight avec les coefficients récupérés des Sliders *)
                     let ct, cd, ca, cc = match coeffs with 
                     |[a;b;c;d] -> a,b,c,d 
                     | _ -> 1.0,1.0,1.0,1.0 in
                     let critere = [
                        Combinator.criterion (module Weight.TimeWeight) ~coefficient:ct ~label:"Temps";
                        Combinator.criterion (module Weight.DistanceWeight) ~coefficient:cd ~label:"Distance";
                        Combinator.criterion (module Weight.HopWeight) ~coefficient:ca ~label:"Arrêts";
                        Combinator.criterion (module Weight.CorrespondenceWeight) ~coefficient:cc ~label:"Correspondances";
                     ] in
                     let module CombinedWeight = (val Combinator.combine critere : Routing.WEIGHT with type edge = Network.connection and type node = Network.station) in
                     let module DIJKSTRA_CombinedWeight = Dijkstra.Make(Network.TransportGraph)(CombinedWeight) in
                     [("Combinaison", (module DIJKSTRA_CombinedWeight))]
                | _                 ->
                    [("Temps", (module DIJKSTRA_TimeWeight))]
              in

          (* Accumulation fonctionnelle des détails d'affichage *)
          let details_afficheurs = List.fold_left (fun acc (nom_algo, (module Routeur : ALGO)) ->
            let module ShowPath = Show.Make (Routeur.P) in
            Printf.printf "\n=== Résultat avec %s ===\n%!" nom_algo;
            match Routeur.find_path ~start_time ~graph:graph_for_search ~source ~destination ~transfer_allowed:(if pmr_transfer then Util.is_pmr else fun _ -> true) with
            | Some p ->
                ShowPath.print_path
                  (fun s -> s.name)
                  (fun (c : Network.connection) -> c.line.name)
                  Routeur.P.Weight.to_string
                  (fun (c : Network.connection) -> c.line.color)
                  (fun (c : Network.connection) -> c.duration)
                  (fun (c : Network.connection) -> c.distance)
                  details_mode
                  p;
                flush stdout;
                (* Créer une fermeture pour afficher le détail *)
                let afficher_detail () =
                  Printf.printf "\n=== Détails du chemin avec %s ===\n%!" nom_algo;
                  ShowPath.print_path_detail
                    (fun s -> s.name)
                    (fun (c : Network.connection) -> c.line.name)
                    Routeur.P.Weight.to_string
                    (fun (c : Network.connection) -> c.line.color)
                    (fun (c : Network.connection) -> c.duration)
                    (fun (c : Network.connection) -> c.distance)
                    p
                in
                (* Ajouter à l'accumulateur *)
                acc @ [(nom_algo, afficher_detail)]
            | None -> 
                Printf.printf "Aucun chemin trouvé avec %s.\n%!" nom_algo;
                acc
          ) [] routeurs in
          
          (* Proposer à l'utilisateur de voir les détails d'un chemin spécifique *)
          if details_afficheurs <> [] then (
            let noms_algos = List.map fst details_afficheurs in
            let choix = Cli.Console.raw_select "\nVoir les détails du chemin avec quel algorithme ?" noms_algos in
            try
              let (_, afficher_detail) = List.find (fun (nom, _) -> nom = choix) details_afficheurs in
              afficher_detail(); flush stdout
            with Not_found -> Printf.printf "Algorithme non trouvé.\n%!"
          )

      | None -> Printf.printf "Erreur: Stations introuvables ou coordonnées invalides.\n%!"
    in

    Printf.printf "\nGraphe chargé avec succès !\n%!";
    Printf.printf "Choisissez l'interface à lancer :\n%!";
    Printf.printf "  1 - Interface Graphique (GUI)\n%!";
    Printf.printf "  2 - Interface Ligne de Commande (CLI)\n%!";

    match Cli.Console.raw_select "" [ "1"; "2" ] with
    | "2" -> 
        if(loading_mode = "En ligne" && town = "Ile-de-France" && rt_url_alertes <> "") then begin
          Printf.printf "\nAlertes en cours :\n%!";
          !Gtfs_rt.liste_alertes |> List.iter (fun alerte -> Printf.printf "Alerte : %s\n%!" alerte.Gtfs_rt.titre);
        end;
        let cli_callback src dest algo poids coeffs excluded_modes start_time =
          search_callback src dest algo poids coeffs excluded_modes start_time false false
        in
        Cli.Console.run_console graphe cli_callback

    | _ ->
        Printf.printf "Lancement de l'interface graphique...\n%!";
        let gui_callback src dest algo poids coeffs excluded_modes start_time pmr_transfer =
          search_callback src dest algo poids coeffs excluded_modes start_time pmr_transfer false
        in
        Gui.Transport_view.affiche graphe gui_callback;

  with
  | Failure msg -> Printf.eprintf "ERREUR : %s\n" msg
  | Sys_error msg -> Printf.eprintf "ERREUR FICHIER : %s\n" msg
  | e -> Printf.eprintf "ERREUR INCONNUE : %s\n" (Printexc.to_string e)
