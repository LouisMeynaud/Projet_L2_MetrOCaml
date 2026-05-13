open Tiktocaml

module DijkstraForce = Routing.Dijkstra.Make (Network.FriendGraph) (Weight.ForceWeight)
module Bfs            = Routing.Bfs.Make     (Network.FriendGraph) (Weight.ForceWeight)

module type TIKTOK_ALGO = Routing.ALGO
  with type graph    = Network.FriendGraph.t
  with type P.Node.t = Network.person
  with type P.Edge.t = Network.connection

let make_heuristic (graph : Network.FriendGraph.t) =
  let force_max =
    List.fold_left (fun acc node ->
      List.fold_left (fun acc' (_, (conn : Network.connection)) ->
        Float.max acc' conn.force
      ) acc (Network.FriendGraph.neighbors graph node)
    ) 0.0 (Network.FriendGraph.nodes graph)
  in
  let force_max = if force_max <= 0.0 then 1.0 else force_max in
  fun (a : Network.person) (b : Network.person) ->
    if String.equal a.name b.name then 0.0 else 1.0 /. force_max

let make_astar h =
  let module H = struct type node = Network.person let estimate = h end in
  (module Routing.Astar.Make (Network.FriendGraph) (Weight.ForceWeight) (H) : TIKTOK_ALGO)

let run_algo (module Algo : TIKTOK_ALGO) graph source destination =
  match Algo.find_path ~init:(Algo.P.Weight.start_at 0) ~graph ~source ~destination ~can_change_state:(fun _ -> true) with
  | None -> Printf.printf "Aucun chemin trouvé.\n"
  | Some p ->
      List.iter (fun (s : Algo.P.step) -> Printf.printf "%s -> %s\n" s.from.name s.to_node.name) p.steps;
      Printf.printf "Poids total : %s\n" (Algo.P.Weight.to_string p.total_weight)

let make_bench_entry (module Algo : TIKTOK_ALGO) =
  fun graph source destination start_time ->
    match Algo.find_path ~init:(Algo.P.Weight.start_at start_time) ~graph ~source ~destination ~can_change_state:(fun _ -> true) with
    | None -> ("Aucun chemin", fun () -> ())
    | Some p -> ("OK", fun () ->
        List.iter (fun (s : Algo.P.step) ->
          Printf.printf "%s -> %s\n" s.from.name s.to_node.name) p.steps)

let () =
  let graph = Graph.create_test_graph () in

  Printf.printf "Algo (dijkstra/a*/bfs/bench) : \n";
  let algo = String.lowercase_ascii (String.trim (read_line ())) in
  Printf.printf "Nom de depart : \n";
  let src_query = String.trim (read_line ()) in
  Printf.printf "Nom d'arrivée : \n";
  let dst_query = String.trim (read_line ()) in

  if algo = "bench" then begin
    let astar = make_astar (make_heuristic graph) in
    let routeurs = [
      ("Dijkstra", make_bench_entry (module DijkstraForce));
      ("A*",       make_bench_entry astar);
      ("BFS",      make_bench_entry (module Bfs));
    ] in
    Bench.run graph src_query dst_query ~init:0 routeurs (fun _ choices -> List.hd choices)
  end else
    match Graph.find_person src_query, Graph.find_person dst_query with
    | None, _ | _, None -> Printf.printf "Nom inconnu.\n"
    | Some source, Some destination ->
        let algo_module : (module TIKTOK_ALGO) = match algo with
          | "bfs"      -> (module Bfs)
          | "dijkstra" -> (module DijkstraForce)
          | _          -> make_astar (make_heuristic graph)
        in
        run_algo algo_module graph source destination
