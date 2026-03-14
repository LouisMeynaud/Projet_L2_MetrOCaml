(* bin/main.ml *)

open Metrocaml
open Weight
open Routing

module DIJKSTRA_CorrespondenceWeight =
  Dijkstra.Make (Network.TransportGraph) (Weight.CorrespondenceWeight)

module DIJKSTRA_DistanceWeight =
  Dijkstra.Make (Network.TransportGraph) (Weight.DistanceWeight)

module DIJKSTRA_TimeWeight =
  Dijkstra.Make (Network.TransportGraph) (Weight.TimeWeight)

module DIJKSTRA_HopWeight =
  Dijkstra.Make (Network.TransportGraph) (Weight.HopWeight)

module BFS_HopWeight = Bfs.Make (Network.TransportGraph) (Weight.HopWeight)

let criteria =
  [
    Combinator.criterion
      (module Weight.TimeWeight)
      ~coefficient:0.5 ~label:"Temps";
    Combinator.criterion
      (module Weight.CorrespondenceWeight)
      ~coefficient:0.3 ~label:"Correspondances";
    Combinator.criterion
      (module Weight.HopWeight)
      ~coefficient:0.2 ~label:"Arrêts";
  ]

module CombinedWeight =
  (val Combinator.combine criteria
      : Routing.WEIGHT with type edge = Network.connection)

module DIJKSTRA_CombinedWeight =
  Dijkstra.Make (Network.TransportGraph) (CombinedWeight)

let find_station graphe id =
  List.find_opt
    (fun (s : Network.station) -> s.id = id)
    (Network.TransportGraph.nodes graphe)

let find_station_by_name graphe name =
  List.find_opt
    (fun (s : Network.station) -> s.name = name)
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

let rec raw_select prompt options =
  Printf.printf "%s (%s) : %!" prompt (String.concat " / " options);
  let choice = read_line () in
  if List.mem choice options then choice
  else begin
    Printf.printf
      "Choix invalide. Veuillez taper l'une des options exactement.\n%!";
    raw_select prompt options
  end

let run_console graphe search_callback =
  Printf.printf "\n=== Mode Ligne de Commande ===\n%!";

  Printf.printf "Entrez le nom de la station source : %!";
  let source_name = read_line () in

  Printf.printf "Entrez le nom de la station destination : %!";
  let dest_name = read_line () in

  let get_station_id name =
    match find_station_by_name graphe name with
    | Some station -> station.id
    | None -> ""
  in

  let source_id = get_station_id source_name in
  let dest_id = get_station_id dest_name in

  let algo = raw_select "Choisissez l'algorithme : " [ "BFS"; "Dijkstra" ] in

  let poids =
    if algo = "Dijkstra" then begin
      raw_select "Choisissez le poids : "
        [ "Distance"; "Arrets"; "Temps"; "Correspondances" ]
    end
    else ""
  in

  search_callback source_id dest_id algo poids

let () =
  (*let montpellier = "https://www.data.gouv.fr/api/1/datasets/r/85ef3b33-2289-4d19-b479-276e5b1c5a52" in*)
  let toulouse =
    "https://www.data.gouv.fr/api/1/datasets/r/64318177-f5b5-4144-a6be-2d0f22d26c77"
  in
  (*let marseille = "https://www.data.gouv.fr/api/1/datasets/r/ab931edc-0bc1-4c66-960a-dc546b46df03" in*)
  (* test jour férié 01/05 *)
  let date_cible = "20260312" in
  let heure_min = 8 * 3600 in

  Printf.printf "=== Lancement de MetroCaml ===\n%!";

  try
    let graphe =
      Gtfs_adapter.load_network_from_url toulouse date_cible heure_min
    in

    flush stdout;

    let search_callback source_id dest_id algo poids =
      match (find_station graphe source_id, find_station graphe dest_id) with
      | Some source, Some destination -> (
          let (module Routeur : ALGO) =
            if algo = "BFS" then (module BFS_HopWeight)
            else
              match poids with
              | "Distance" -> (module DIJKSTRA_DistanceWeight)
              | "Arrets" -> (module DIJKSTRA_HopWeight)
              | "Temps" -> (module DIJKSTRA_TimeWeight)
              | "Correspondances" -> (module DIJKSTRA_CorrespondenceWeight)
              | "Combinaison" -> (module DIJKSTRA_CombinedWeight)
              | _ -> (module DIJKSTRA_TimeWeight)
          in
          let module ShowPath = Show.Make (Routeur.P) in
          match Routeur.find_path ~graph:graphe ~source ~destination with
          | Some p ->
              ShowPath.print_path
                (fun s -> s.name)
                (fun (c : Network.connection) -> c.line.name)
                Routeur.P.Weight.to_string
                (fun (c : Network.connection) -> c.line.color)
                p;
              flush stdout
          | None -> Printf.printf "Aucun chemin trouvé.\n%!")
      | _ -> Printf.printf "Erreur: Stations introuvables.\n%!"
    in

    Printf.printf "\nGraphe chargé avec succès !\n%!";
    Printf.printf "Choisissez l'interface à lancer :\n%!";
    Printf.printf "  1 - Interface Graphique (GUI)\n%!";
    Printf.printf "  2 - Interface Ligne de Commande (CLI)\n%!";

    match raw_select "" [ "1"; "2" ] with
    | "2" -> run_console graphe search_callback
    | _ ->
        Printf.printf "Lancement de l'interface graphique...\n%!";
        Graphics.set_window_title "MetroCaml - Réseau";
        Gui.Transport_view.affiche graphe search_callback
  with
  | Failure msg -> Printf.eprintf "ERREUR : %s\n" msg
  | Sys_error msg -> Printf.eprintf "ERREUR FICHIER : %s\n" msg
  | e -> Printf.eprintf "ERREUR INCONNUE : %s\n" (Printexc.to_string e)
