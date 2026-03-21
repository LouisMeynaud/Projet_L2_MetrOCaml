(* bin/main.ml *)

open Metrocaml
open Weight
open Routing

module DIJKSTRA_CorrespondenceWeight = Dijkstra.Make(Network.TransportGraph)(Weight.CorrespondenceWeight)
module DIJKSTRA_DistanceWeight       = Dijkstra.Make(Network.TransportGraph)(Weight.DistanceWeight)
module DIJKSTRA_TimeWeight           = Dijkstra.Make(Network.TransportGraph)(Weight.TimeWeight)
module DIJKSTRA_HopWeight            = Dijkstra.Make(Network.TransportGraph)(Weight.HopWeight)
module BFS_HopWeight                 = Bfs.Make(Network.TransportGraph)(Weight.HopWeight)

let find_station graphe id =
  List.find_opt
    (fun (s : Network.station) -> s.id = id)
    (Network.TransportGraph.nodes graphe)

module type ALGO = sig
  module P :
    Routing.PATH
      with type Node.t = Network.station
       and type Edge.t = Network.connection

  val find_path :
    graph:Network.TransportGraph.t ->
    source:Network.station ->
    destination:Network.station ->
    P.t option
end

let today_yyyymmdd () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

let () =
  let town = raw_select "Choisissez la ville : " [ "Toulouse"; "Montpellier"; "Marseille"; "Ile-de-France" ] in
  let url =
    match town with
    | "Toulouse" -> "https://www.data.gouv.fr/api/1/datasets/r/64318177-f5b5-4144-a6be-2d0f22d26c77"
    | "Montpellier" -> "https://www.data.gouv.fr/api/1/datasets/r/85ef3b33-2289-4d19-b479-276e5b1c5a52"
    | "Marseille" -> "https://www.data.gouv.fr/api/1/datasets/r/ab931edc-0bc1-4c66-960a-dc546b46df03"
    | "Ile-de-France" -> "https://eu.ftp.opendatasoft.com/stif/GTFS/IDFM-gtfs.zip"
    | _ -> failwith "Ville inconnue"
  in

  let date_cible = today_yyyymmdd () in
  let heure_min = 8 * 3600 in  

  Printf.printf "=== Lancement de MetroCaml ===\n%!";
  Printf.printf "Date GTFS cible: %s\n%!" date_cible;

  try
    let graphe = Gtfs_adapter.load_network_from_url url date_cible heure_min in
    flush stdout;
    
    let search_callback source_id dest_id algo poids coeffs =
      match find_station graphe source_id, find_station graphe dest_id with
      | Some source, Some destination ->

          let (module Routeur : ALGO) =
            if algo = "BFS" then (module BFS_HopWeight)
            else
<<<<<<< bin/main.ml
              match poids with
                | "Distance"        -> (module DIJKSTRA_DistanceWeight)
                | "Arrets"          -> (module DIJKSTRA_HopWeight)
                | "Temps"           -> (module DIJKSTRA_TimeWeight)
                | "Correspondances" -> (module DIJKSTRA_CorrespondenceWeight)
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
                     let module CombinedWeight = (val Combinator.combine critere : Routing.WEIGHT with type edge = Network.connection) in
                     let module DIJKSTRA_CombinedWeight = Dijkstra.Make(Network.TransportGraph)(CombinedWeight) in
                     (module DIJKSTRA_CombinedWeight)
                | _                 -> (module DIJKSTRA_TimeWeight)
              in
          let module ShowPath = Show.Make (Routeur.P) in
          match Routeur.find_path ~graph:graphe ~source ~destination with
          | Some p ->
              ShowPath.print_path
                (fun s -> s.name)
                (fun (c : Network.connection) -> c.line.name)
                Routeur.P.Weight.to_string
                (fun (c : Network.connection) -> c.line.color)
                (fun (c : Network.connection) -> c.duration)
                (fun (c : Network.connection) -> c.distance)
                (Printf.printf
                   "Choisissez l'affichage que vous voulez (1 : Normal) (2 : \
                    Détaillés) \n\
                    %!";
                 match raw_select "" [ "1"; "2" ] with
                 | "1" -> false
                 | _ -> true)
                p;
              flush stdout
          | None -> Printf.printf "Aucun chemin trouvé.\n%!")
      | _ -> Printf.printf "Erreur: Stations introuvables.\n%!"
    in

    Printf.printf "\nGraphe chargé avec succès !\n%!";
    Printf.printf "Choisissez l'interface à lancer :\n%!";
    Printf.printf "  1 - Interface Graphique (GUI)\n%!";
    Printf.printf "  2 - Interface Ligne de Commande (CLI)\n%!";

    match Cli.Console.raw_select "" [ "1"; "2" ] with
    | "2" -> Cli.Console.run_console graphe search_callback
    | _ ->
        Printf.printf "Lancement de l'interface graphique...\n%!";
        Gui.Transport_view.affiche graphe search_callback;

  with
  | Failure msg -> Printf.eprintf "ERREUR : %s\n" msg
  | Sys_error msg -> Printf.eprintf "ERREUR FICHIER : %s\n" msg
  | e -> Printf.eprintf "ERREUR INCONNUE : %s\n" (Printexc.to_string e)
  