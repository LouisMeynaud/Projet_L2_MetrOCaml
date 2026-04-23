open Tiktocaml

module DijkstraForce = Routing.Dijkstra.Make (Network.FriendGraph) (Weight.Forceheight)
module Bfs = Routing.Bfs.Make (Network.FriendGraph) (Weight.Forceheight)

let find_person name =
  let name_ = String.lowercase_ascii (String.trim name) in
  List.find_opt (fun (p : Network.person) -> String.lowercase_ascii p.name = name_) Graphe.people

let print_steps steps =
  List.iter
    (fun (s : _ ) -> Printf.printf "%s -> %s\n" s.from.name s.to_node.name)
    steps

let run_bfs graph source destination =
  match Bfs.find_path ~start_time:0 ~graph ~source ~destination with
  | None -> Printf.printf "Aucun chemin trouvé.\n"
  | Some p ->
      print_steps p.steps;
      Printf.printf "poids total: %s\n" (Bfs.P.Weight.to_string p.total_weight)

let run_dijkstra graph source destination =
  match
    DijkstraForce.find_path ~start_time:0 ~graph ~source ~destination
      ~transfer_allowed:(fun _ -> true)
  with
  | None -> Printf.printf "Aucun chemin trouve.\n"
  | Some p ->
      print_steps p.steps;
      Printf.printf "poids total: %s\n"
        (DijkstraForce.P.Weight.to_string p.total_weight)

let make_routeur name run_path =
  ( name,
    fun g source destination start_time ->
      match run_path ~start_time ~graph:g ~source ~destination with
      | None -> ("Aucun chemin", fun () -> ())
      | Some p ->
          ( "OK",
            fun () ->
              print_steps p.steps ) )

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
        make_routeur "Dijkstra"
          (fun ~start_time ~graph ~source ~destination ->
            DijkstraForce.find_path ~start_time ~graph ~source ~destination
              ~transfer_allowed:(fun _ -> true));
        make_routeur "BFS"
          (fun ~start_time ~graph ~source ~destination ->
            Bfs.find_path ~start_time ~graph ~source ~destination);
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