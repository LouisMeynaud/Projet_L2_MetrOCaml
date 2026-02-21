(* bin/main.ml *)

open Metrocaml
open Routing

module DIJKSTRA = Dijkstra.Make(Network.TransportGraph)(Weight.TransferWeight)

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
    
    let search_callback source_id dest_id =
      match find_station graphe source_id, find_station graphe dest_id with
      | Some source, Some destination ->
        (match DIJKSTRA.find_path ~graph:graphe ~source ~destination with
        | Some p ->
          Printf.printf "Chemin trouvé en %d étapes :\n%!" (List.length p.steps);
          List.iter (fun (step : DIJKSTRA.P.step) ->
            Printf.printf "  %s -> %s\n%!" step.from.name step.to_node.name
          ) p.steps
        | None -> Printf.printf "Aucun chemin trouvé.\n%!")
      | _ -> Printf.printf "Erreur: Stations introuvables.\n%!"
    in

    Graphics.set_window_title "MetroCaml - Réseau";
    Gui.Transport_view.affiche graphe search_callback;
  with
  | Failure msg -> Printf.eprintf "ERREUR : %s\n" msg
  | Sys_error msg -> Printf.eprintf "ERREUR FICHIER : %s\n" msg
  | e -> Printf.eprintf "ERREUR INCONNUE : %s\n" (Printexc.to_string e)