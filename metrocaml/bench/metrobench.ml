open Metrocaml
open Routing
open Weight

module BFS_HopWeight = Bfs.Make (Network.TransportGraph) (Weight.HopWeight)

module type ALGO = Routing.ALGO
  with type graph = Network.TransportGraph.t
  with type P.Node.t = Network.station
  with type P.Edge.t = Network.connection


(** Paramétrage des types *)

type stat = {
  total_time : int;
  transfers : int;
  arrival_time : int; 
  nb_steps : int;
}

type routeur =
  Network.TransportGraph.t -> Network.station -> Network.station -> int -> (stat option) * (unit -> unit)



(** Création d'un routeur avec son nom associé *)
let make_routeur nom (module Algo : ALGO) = 
  let compute_route graph source destination start_time =
    match Algo.find_path ~start_time ~graph ~source ~destination ~transfer_allowed:(fun _ -> true) with
    | None -> (None, fun () -> ())
    | Some p -> let module ShowPath = Show.Make (Algo.P) in
        let stats_show = ShowPath.compute_summary ~start_time p in
        let stats = { total_time = stats_show.total_time;  transfers = stats_show.transfers; arrival_time = stats_show.arrival_time; nb_steps = List.length p.Algo.P.steps } in

        (Some stats, fun () -> Printf.printf "%s\n" (ShowPath.string_of_path ~start_time ~use_color:true p))
  in
  (nom, compute_route)



(** Renvoie le bon type pour le benchmark générique unique *)
let print_bench_unique (nom, compute) = 

  let compute_route graph_for_search source destination start_time= 
    let result, (print_detail : unit -> unit) = compute graph_for_search source destination start_time in
    let result_str = match result with
      | None -> "Aucun chemin"
      | Some stats -> Printf.sprintf
          "OK - %d Etape(s)     %d Heures %d Minutes     Arrivé à %02d:%02d     en %d Correspondances"
          stats.nb_steps
          (stats.total_time / 3600) (stats.total_time / 60 mod 60)
          (stats.arrival_time / 3600 mod 24) (stats.arrival_time / 60 mod 60)
          stats.transfers
    in
    (result_str, print_detail)
  in 
  (nom, compute_route)

(** Création d'un routeur pour le benchmark unique *)
let bench_routeur_unique nom (module Algo : ALGO) = 
  print_bench_unique (make_routeur nom (module Algo))


(** Benchmark unique *)
let run_bench_unique graph routeurs =
  Printf.printf "\nBenchmark activé \n%!";
  Random.self_init ();
  Printf.printf "\nStation source : (laissez vide pour choisir aléatoirement) %!";
  let source_query = String.trim (read_line ()) in
  Printf.printf "Station destination : (laissez vide pour choisir aléatoirement) %!";
  let dest_query = String.trim (read_line ()) in
  let src, dst = match source_query, dest_query with
  | "", "" -> (match Bench.pick_random_pair (Network.TransportGraph.nodes graph) with
    | Some (src, dst) -> (src.name, dst.name)
    | None -> Printf.printf "Aucune paire de stations aléatoires valides pour le benchmark \n%!"; ("", ""))
  | "", dst -> let src =  Bench.pick_random_station (Network.TransportGraph.nodes graph) 
    in (match src with 
    |Some src ->  (src.name, dst)
    |None -> Printf.printf "Aucune station source aléatoire valide pour le benchmark \n%!"; ("", dst))
  | src, "" -> let dst = Bench.pick_random_station (Network.TransportGraph.nodes graph)
    in (match dst with 
    |Some dst ->  (src, dst.name)
    |None -> Printf.printf "Aucune station destination aléatoire valide pour le benchmark \n%!"; (src, ""))
  | src, dst -> (src, dst)
  in let tm = Unix.localtime (Unix.time ()) in
  let start_time = tm.Unix.tm_hour * 3600 + tm.Unix.tm_min * 60 in
  Routing.Bench.run
    ~title:"Benchmark MetroCaml"
    ~resolve:(fun g src dst -> Endpoint.build_routing_context g src dst)
    graph src dst start_time routeurs Cli.Console.raw_select


(** Bench aléatoire*)

(** Fonctions pour les moyennes *)
let avg_elapsed l = 
  if List.length l = 0 then 0.0
  else List.fold_left (+.) 0.0 l /. float_of_int (List.length l)

let avg_weight l = 
  if List.length l = 0 then (0.0, 0.0)
  else let time_avg, transfers_avg = List.fold_left 
  (fun (time_acc, transfers_acc) w -> match w with
   |None -> time_acc, transfers_acc
   |Some stat -> (time_acc +. (float_of_int stat.total_time), transfers_acc +. (float_of_int stat.transfers))) 
   (0.0, 0.0) l in
  (time_avg /. float_of_int (List.length l), transfers_avg /. float_of_int (List.length l))

(** Fonctions pour exécuter les routeurs et calculer leurs bench/stats *)
let run_routeur (name, compute) graph source destination start_time =
  let t0 = Unix.gettimeofday () in
  let exception Timeout in
  let timeout_seconds = 30.0 in
  let old_handler = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
  try
    let _ = Unix.setitimer Unix.ITIMER_REAL {it_interval = 0.0; it_value = timeout_seconds} in 
    let stats, _ = compute graph source destination start_time in
    let _ = Unix.setitimer Unix.ITIMER_REAL {it_interval = 0.0; it_value = 0.0} in
    let elapsed = Unix.gettimeofday () -. t0 in
    let _ = Sys.signal Sys.sigalrm old_handler in
    (stats, elapsed)
  with Timeout ->
    let _ =  Unix.setitimer Unix.ITIMER_REAL {it_interval = 0.0; it_value = 0.0} in 
    let elapsed = Unix.gettimeofday () -. t0 in
    let _ = Sys.signal Sys.sigalrm old_handler in
    Printf.printf "Timeout dépassé pour %s (%.1f sec) - Aucun chemin trouvé\n%!" name elapsed;
    (None, elapsed)
  | e ->
    let _ = Unix.setitimer Unix.ITIMER_REAL {it_interval = 0.0; it_value = 0.0} in
    let _ = Sys.signal Sys.sigalrm old_handler in
    raise e

(** Validation d'une paire de routeurs en cherchant le chemin d'un nœud à un autre avec un algorithme donné *)
let validate_pair (module Algo : ALGO) graph source destination start_time =
  match
    Algo.find_path ~start_time ~graph ~source ~destination ~transfer_allowed:(fun _ -> true)
  with
  | None -> None
  | Some path -> Some (List.length path.Algo.P.steps)


(** Module pour utiliser le foncteur MakeRandomBench *)
module MetroCamlRandomBench = struct
  type graph = Network.TransportGraph.t
  type node = Network.station
  type routeur_pair = {
    preset_name : string;
    dijkstra : string * routeur;
    astar : string * routeur;
  }

  (** Récupération des nœuds d'un graphe *)
  let get_nodes graph = Network.TransportGraph.nodes graph

  (**  Trouver des points de départ et d'arrivée pour le benchmark valide*)
  let build_context graph src dst =
    match Endpoint.build_routing_context graph src.Network.id dst.Network.id with
    | None -> None
    | Some (g, s, d) -> Some (g, s, d)

  (** Lancer les benchmarks pour une paire de routeurs et calculer les statistiques *)
  let benchmark_pair routeur_pairs graph source destination start_time =
    match validate_pair (module BFS_HopWeight) graph source destination start_time with
    | None -> None
    | _ ->
        let category = Bench.category_of_distance (int_of_float (Util.haversine_distance source.latitude source.longitude destination.latitude destination.longitude)) in
        let dijkstra_tot_elapsed, astar_tot_elapsed, d_stats, a_stats =
          List.fold_left
            (fun (d_acc, a_acc, d_stats_acc, a_stats_acc) pair ->
              (* Printf.printf "Benchmarking %s\n%!" pair.preset_name; *)
              let d_stats, d_duration = run_routeur pair.dijkstra graph source destination start_time in
              if d_stats == None then 
                (d_acc, a_acc, d_stats_acc, a_stats_acc)
              else 
              (* Printf.printf "Dijkstra: %d steps, elapsed time: %.4f seconds\n%!" (match d_stats with Some s -> s.nb_steps | None -> -1) d_duration; *)
              let a_stats, a_duration = run_routeur pair.astar graph source destination start_time in
              if(a_stats == None) then
                (d_acc, a_acc, d_stats_acc, a_stats_acc)
              else
              (* Printf.printf "A*: %d steps, elapsed time: %.4f seconds\n%!" (match a_stats with Some s -> s.nb_steps | None -> -1) a_duration; *)
              (d_duration :: d_acc, a_duration :: a_acc, d_stats :: d_stats_acc, a_stats :: a_stats_acc))
            ([], [], [], []) routeur_pairs
        in
        let dijkstra_elapsed_avg = avg_elapsed dijkstra_tot_elapsed in
        let astar_elapsed_avg = avg_elapsed astar_tot_elapsed in
        let dijkstra_time, dijkstra_transfer = avg_weight d_stats in
        let astar_time, astar_transfer = avg_weight a_stats in
        let dijkstra_win_elapsed = if dijkstra_elapsed_avg < astar_elapsed_avg then 1 else 0 in
        let dijkstra_win_time = if dijkstra_time <= astar_time then 1 else 0 in
        let dijkstra_win_transfer = if dijkstra_transfer <= astar_transfer then 1 else 0 in
        let dijkstra_win_w = dijkstra_win_time + dijkstra_win_transfer in
        Some (category, dijkstra_win_elapsed, dijkstra_win_w, dijkstra_elapsed_avg, astar_elapsed_avg, dijkstra_transfer, astar_transfer)
end


module type CreateRouteurPairs = sig
  val make_routeur_pairs : Network.TransportGraph.t -> MetroCamlRandomBench.routeur_pair list
end

module MakeMetroCamlBench (Routeurs : CreateRouteurPairs) = struct
  module RandomBench = struct
    include MetroCamlRandomBench
    let make_routeur_pairs = Routeurs.make_routeur_pairs
  end
  module Bench = Bench.MakeRandomBench(RandomBench)
end

let run_random_bench_console (module Routeurs : CreateRouteurPairs) graph =
  let module B = MakeMetroCamlBench(Routeurs) in
  B.Bench.run_random_bench_console graph