(* bin/main.ml *)

open Metrocaml
open Routing

module DIJKSTRA_CorrespondenceWeight = Dijkstra.Make(Network.TransportGraph)(Weight.CorrespondenceWeight)
module DIJKSTRA_DistanceWeight       = Dijkstra.Make(Network.TransportGraph)(Weight.DistanceWeight)
module DIJKSTRA_TimeWeight           = Dijkstra.Make(Network.TransportGraph)(Weight.TimeWeight)
module DIJKSTRA_HopWeight            = Dijkstra.Make(Network.TransportGraph)(Weight.HopWeight)
module BFS_HopWeight                 = Bfs.Make(Network.TransportGraph)(Weight.HopWeight)

let find_station graphe id =
  List.find_opt (fun (s : Network.station) -> s.id = id) (Network.TransportGraph.nodes graphe)


module type ALGO = sig
  module P : Routing.PATH 
    with type node = Network.station 
     and type edge = Network.connection 

  val find_path : graph:Network.TransportGraph.t -> source:Network.station -> destination:Network.station -> P.t option
end

let () =
  (*let montpellier = "https://www.data.gouv.fr/api/1/datasets/r/85ef3b33-2289-4d19-b479-276e5b1c5a52" in*)
  let toulouse = "https://www.data.gouv.fr/api/1/datasets/r/64318177-f5b5-4144-a6be-2d0f22d26c77" in
  let date_cible = "20260419" in 
  let heure_min = 8 * 3600 in  

  print_endline "=== Lancement de MetroCaml ===";

  try
    let graphe = Gtfs_adapter.load_network_from_url toulouse date_cible heure_min in
    
    Printf.printf "Graphe chargé ! Lancement de l'interface graphique...\n";
    flush stdout;
    
    let search_callback source_id dest_id algo poids =
      match find_station graphe source_id, find_station graphe dest_id with
      | Some source, Some destination ->

          let (module Routeur : ALGO) =
            if algo = "BFS" then
              (module BFS_HopWeight)
            else
                match poids with
                | "Distance"        -> (module DIJKSTRA_DistanceWeight)
                | "Arrets"          -> (module DIJKSTRA_HopWeight)
                | "Temps"           -> (module DIJKSTRA_TimeWeight)
                | "Correspondances" -> (module DIJKSTRA_CorrespondenceWeight)
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