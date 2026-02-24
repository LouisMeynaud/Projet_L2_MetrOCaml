(* bin/main.ml *)

open Metrocaml

let () =
  let date_cible = "20260419" in 
  let heure_min = 8 * 3600 in  

  print_endline "=== Lancement de MetroCaml ===";

  try
    let graphe = Gtfs_adapter.load_network_from_url "https://www.data.gouv.fr/api/1/datasets/r/85ef3b33-2289-4d19-b479-276e5b1c5a52" date_cible heure_min in
    
    Printf.printf "Graphe chargé ! Lancement de l'interface graphique...\n";
    flush stdout;
    Gui.Transport_view.affiche graphe;
  with
  | Failure msg -> Printf.eprintf "ERREUR : %s\n" msg
  | Sys_error msg -> Printf.eprintf "ERREUR FICHIER : %s\n" msg
  | e -> Printf.eprintf "ERREUR INCONNUE : %s\n" (Printexc.to_string e)