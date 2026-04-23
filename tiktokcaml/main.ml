open Tiktocaml

module DijkstraForce = Routing.Dijkstra.Make (Network.FriendGraph) (Weight.Forceheight)
module Bfs = Routing.Bfs.Make (Network.FriendGraph) (Weight.Forceheight)

let find_person name =
  let clean = String.lowercase_ascii (String.trim name) in
  List.find_opt (fun (p : Network.person) -> String.lowercase_ascii p.name = clean) Graphe.people

let run_bfs graph source destination =
  match Bfs.find_path ~start_time:0 ~graph ~source ~destination with
  | None -> Printf.printf "Aucun chemin trouvé.\n"
  | Some p ->
      List.iter (fun (s : Bfs.P.step) -> Printf.printf "%s -> %s\n" s.from.name s.to_node.name) p.steps;
      Printf.printf "poids total: %s\n" (Bfs.P.Weight.to_string p.total_weight)

let run_dijkstra graph source destination =
  match DijkstraForce.find_path ~start_time:0 ~graph ~source ~destination ~transfer_allowed:(fun _ -> true) with
  | None -> Printf.printf "Aucun chemin trouve.\n"
  | Some p ->
      List.iter (fun (s : DijkstraForce.P.step) -> Printf.printf "%s -> %s\n" s.from.name s.to_node.name) p.steps;
      Printf.printf "Poids total: %s\n" (DijkstraForce.P.Weight.to_string p.total_weight)

let () =
  let graph = Graphe.create_test_graph () in

  Printf.printf "Algo (dijkstra/bfs/bench) : \n";
  let algo = String.lowercase_ascii (String.trim (read_line ())) in

  Printf.printf "Nom de depart : \n";
  let src_query = String.trim (read_line ()) in

  Printf.printf "nom d'arrive : \n";
  let dst_query = String.trim (read_line ()) in

  if algo = "bench" then
    let routeurs =
      [
        ("Dijkstra", fun graph source destination start_time ->
          match DijkstraForce.find_path ~start_time ~graph ~source ~destination ~transfer_allowed:(fun _ -> true) with
          | None -> ("Aucun chemin", fun () -> ())
          | Some p -> ("OK", fun () ->
              List.iter (fun (s : DijkstraForce.P.step) -> 
                Printf.printf "%s -> %s\n" s.from.name s.to_node.name) p.steps)
        );
        
        ("BFS", fun graph source destination start_time ->
          match Bfs.find_path ~start_time ~graph ~source ~destination with
          | None -> ("Aucun chemin", fun () -> ())
          | Some p -> ("OK", fun () ->
              List.iter (fun (s : Bfs.P.step) -> 
                Printf.printf "%s -> %s\n" s.from.name s.to_node.name) p.steps)
        );
      ]
    in
    Bench.run graph src_query dst_query 0 routeurs (fun _ choices -> List.hd choices)
  else
    let src = find_person src_query in
    let dst = find_person dst_query in

    match src, dst with
    | None, _ | _, None ->
        Printf.printf " nom inconnu\n"
    | Some source, Some destination ->
        if algo = "bfs" then run_bfs graph source destination
        else run_dijkstra graph source destination