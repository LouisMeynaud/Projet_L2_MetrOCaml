(* bin/main.ml *)

open Metrocaml
open Weight
open Routing

module DIJKSTRA = Dijkstra.Make(Network.TransportGraph)(TimeWeight)

let find_station graphe id =
  List.find_opt (fun (s : Network.station) -> s.id = id) (Network.TransportGraph.nodes graphe)

let () =
  let dossier_data = "data/Toulouse" in
  let date_cible = "20260419" in 
  let heure_min = 8 * 3600 in  

  print_endline "=== Lancement de MetroCaml ===";

  try
    let graphe = Gtfs_adapter.load_network_gtfs dossier_data date_cible heure_min in
    
    Printf.printf "Graphe chargé ! Lancement de l'interface graphique...\n";
    flush stdout;
    let source_id = "stop_point:SP_100" in
    let dest_id = "stop_point:SP_1000" in
    (match find_station graphe source_id, find_station graphe dest_id with
    | None, _ -> Printf.printf "Station source introuvable : %s\n" source_id
    | _, None -> Printf.printf "Station destination introuvable : %s\n" dest_id
    | Some source, Some destination ->
      (match DIJKSTRA.find_path ~graph:graphe ~source ~destination with
      | Some p -> DIJKSTRA.P.print_path (fun s -> s.name) TimeWeight.to_string p
      | None -> Printf.printf "Aucun chemin trouvé.\n"));
    Graphics.set_window_title "MetroCaml - Réseau";
    Gui.Transport_view.affiche graphe;
  with
  | Failure msg -> Printf.eprintf "ERREUR : %s\n" msg
  | Sys_error msg -> Printf.eprintf "ERREUR FICHIER : %s\n" msg
  | e -> Printf.eprintf "ERREUR INCONNUE : %s\n" (Printexc.to_string e)