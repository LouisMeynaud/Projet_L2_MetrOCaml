(* bin/main.ml *)

open Metrocaml
open Routing
module DIJKSTRA_TransferWeight = Dijkstra.Make(Network.TransportGraph)(Weight.TransferWeight)
module DIJKSTRA_DistanceWeight = Dijkstra.Make(Network.TransportGraph)(Weight.DistanceWeight)
module DIJKSTRA_TimeWeight =     Dijkstra.Make(Network.TransportGraph)(Weight.TimeWeight)
module Dijkstra_hopWeight =      Dijkstra.Make(Network.TransportGraph)(Weight.HopWeight)
module Bfs_hopWeight =           Bfs.Make(Network.TransportGraph)(Weight.HopWeight)

let find_station graphe id =
  List.find_opt (fun (s : Network.station) -> s.id = id) (Network.TransportGraph.nodes graphe)


module type ALGO = sig
  module P : Routing.PATH 
    with type node = Network.station 
     and type edge = Network.connection 

  val find_path : graph:Network.TransportGraph.t -> source:Network.station -> destination:Network.station -> P.t option
end

let () =
  let date_cible = "20260419" in 
  let heure_min = 8 * 3600 in  

  print_endline "=== Lancement de MetroCaml ===";

  try
    let graphe = Gtfs_adapter.load_network_from_url "https://www.data.gouv.fr/api/1/datasets/r/85ef3b33-2289-4d19-b479-276e5b1c5a52" date_cible heure_min in
    
    Printf.printf "Graphe chargé ! Lancement de l'interface graphique...\n";
    flush stdout;
    
    let search_callback source_id dest_id algo poids =
      match find_station graphe source_id, find_station graphe dest_id with
      | Some source, Some destination ->

          let (module Routeur : ALGO) =
            if algo = "BFS" then
              (module Bfs_hopWeight)
            else
                match poids with
                | "Distance"        -> (module DIJKSTRA_DistanceWeight)
                | "Arrets"          -> (module Dijkstra_hopWeight)
                | "Correspondances" -> (module DIJKSTRA_TransferWeight)
                | _                 -> (module DIJKSTRA_TimeWeight)
              in
          (match Routeur.find_path ~graph:graphe ~source ~destination with
          | Some p ->
              Printf.printf "Chemin trouvé en %d étapes :\n%!" (List.length p.Routeur.P.steps);
              List.iter (fun (step : Routeur.P.step) -> 
                Printf.printf "  %s -> %s\n%!" step.from.name step.to_node.name
              ) p.Routeur.P.steps
          | None -> Printf.printf "Aucun chemin trouvé.\n%!")
      | _ -> Printf.printf "Erreur: Stations introuvables.\n%!"
    in

    Graphics.set_window_title "MetroCaml - Réseau";
    Gui.Transport_view.affiche graphe search_callback;
  with
  | Failure msg -> Printf.eprintf "ERREUR : %s\n" msg
  | Sys_error msg -> Printf.eprintf "ERREUR FICHIER : %s\n" msg
  | e -> Printf.eprintf "ERREUR INCONNUE : %s\n" (Printexc.to_string e)