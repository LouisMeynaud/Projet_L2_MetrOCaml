(* bin/main.ml *)

open Metrocaml

let () =
  let dossier_data = "data/Toulouse" in
  let date_cible = "20260419" in 
  let heure_min = 8 * 3600 in  

  print_endline "=== Lancement de MetroCaml ===";

  try
    let graphe = Gtfs_adapter.load_network_gtfs dossier_data date_cible heure_min in
    
    Printf.printf "Graphe chargé ! Lancement de l'interface graphique...\n";
    flush stdout;
    Graphics.open_graph " 20000x10000";
    Graphics.set_window_title "MetroCaml - Réseau";
    Gui.Transport_view.affiche graphe;
  with
  | Failure msg -> Printf.eprintf "ERREUR : %s\n" msg
  | Sys_error msg -> Printf.eprintf "ERREUR FICHIER : %s\n" msg
  | e -> Printf.eprintf "ERREUR INCONNUE : %s\n" (Printexc.to_string e)