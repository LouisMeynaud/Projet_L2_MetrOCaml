let separator () =
  print_string (String.make 130 '-');
  print_newline ()

let format_duration secs =
  let ms = secs *. 1000.0 in
  Printf.sprintf "%.3f ms" ms



(** Catégories pour les stats des benchmarks*)  
let category_of_distance distance =
  if distance < 3000 then "< 3000"
  else if distance < 8000 then "[3000, 8000]"
  else if distance < 13000 then "[8000, 13000]"
  else if distance < 20000 then "[13000, 20000]"
  else ">= 20000"

(** Initialisation des statistiques par catégorie avec la création de la hash  table*)
let init_category_stats () =
  let table = Hashtbl.create 5 in
  List.iter (fun name -> Hashtbl.add table name (0, 0, 0, 0.0, 0.0, 0.0, 0.0)) 
  [ "< 3000"; "[3000, 8000]"; "[8000, 13000]"; "[13000, 20000]"; ">= 20000" ];
  table


(** Mise à jour des statistiques par catégorie *)
let update_category_stats table category d_avg_elapsed a_avg_elapsed d_avg_w a_avg_w d_time_elapsed_win d_time_transfer_win =
  let count, elapsed_wins, time_transfer_wins,  d_total_t, a_total_t, d_total_w, a_total_w = Hashtbl.find table category in
  Hashtbl.replace table category (count + 1, elapsed_wins + d_time_elapsed_win, time_transfer_wins + d_time_transfer_win, d_total_t +. d_avg_elapsed, a_total_t +. a_avg_elapsed, d_total_w +. d_avg_w, a_total_w +. a_avg_w)

(** Filtrer une liste de nœuds en fonction d'une taille maximale *)
let filter_nodes nodes max_size =
  let size = min (List.length nodes) max_size in
  let split = 
    if size < List.length nodes then
      (List.filteri (fun i _ -> i < size) nodes)
    else
      nodes
    in split

(** Sélectionner une station aléatoire dans une liste de nœuds *)
let pick_random_station nodes =
  if nodes = [] then None
  else
    Some (List.nth nodes (Random.int (List.length nodes)))
let pick_random_pair nodes_filtered =
  match (pick_random_station nodes_filtered, pick_random_station nodes_filtered) with
  | Some src, Some dst when src <> dst -> Some (src, dst)
  | _ -> None

(** Afficher les résultats du benchmark aléatoire *)
let print_random_bench_results stats =
  Printf.printf "\n\n=== RÉSULTATS DU BENCHMARK ALÉATOIRE ===\n\n%!";
  Printf.printf "%-14s | %-10s | %-10s | %-10s | %-12s | %-12s | %-12s | %-12s\n%!"
    "Catégorie" "Bench" "Dijkstra TW" "Dijkstra WW" "Dijkstra ms" "A* ms" "Dijkstra w" "A* w";
  separator ();
  let categories = ["< 3000"; "[3000, 8000]"; "[8000, 13000]"; "[13000, 20000]"; ">= 20000"] in
  let total_elapsed_wins, total_time_transfer_wins, total_count =
    List.fold_left
      (fun (te, tw ,tc) cat ->
        let count, elapsed_wins, time_transfer_wins, dtotal, atotal, dtotal_w, atotal_w = Hashtbl.find stats cat in
        let calcul_avg arg = if count = 0 then 0.0 else arg /. float_of_int count in
        let average_d = calcul_avg dtotal in
        let average_a = calcul_avg atotal in
        let average_d_w = calcul_avg dtotal_w in
        let average_a_w = calcul_avg atotal_w in
        Printf.printf "%-14s | %10d | %9.1f%% | %9.1f%% | %12.4f | %12.4f | %12.4f | %12.4f\n%!" cat count
          (calcul_avg (float_of_int elapsed_wins) *. 100.0)
          (calcul_avg (float_of_int time_transfer_wins) *. 100.0)
          average_d average_a average_d_w average_a_w;
        (te + elapsed_wins, tw + time_transfer_wins, tc + count)) (0, 0, 0) categories
  in
  separator ();
  let global_percentage_elapsed = if total_count = 0 then 0.0 else (float_of_int total_elapsed_wins /. float_of_int total_count) *. 100.0 in
  Printf.printf "\n Total_time_elasped_wins : %d , Total_time_transfers_wins : %d , total_count : %d\n%!" total_elapsed_wins total_time_transfer_wins total_count;
  let global_percentage_time_transfer = if total_count = 0 then 0.0 else (float_of_int total_time_transfer_wins /. float_of_int total_count) *. 100.0 in
  Printf.printf "\nStatistique Globale: Dijkstra meilleur dans %.2f%% en terme de temps/chrono et dans %.2f%% des cas en terme de poids (Time / Transfers) \n\n%!" global_percentage_elapsed global_percentage_time_transfer



(** Exécuter le benchmark unique *)
let run ~title ~resolve graph source_query dest_query ~init routeurs raw_select =
  Printf.printf "\n%s de %s à %s\n%!" title source_query dest_query;
  match resolve graph source_query dest_query with
  | None -> Printf.printf "Entrées introuvables, benchmark annulé.\n%!"
  | Some (graph_for_search, source, destination) ->
      Printf.printf "\n%-30s  %-12s  %s\n%!" "Algorithme/Preset"
        "Temps" "Resultat";
      separator ();

      let afficheurs =
        List.filter_map
          (fun (nom, compute) ->
            let t0 = Unix.gettimeofday () in
            let result_str, (print_detail) =
              compute graph_for_search source destination init
            in
            let t1 = Unix.gettimeofday () in
            Printf.printf "%-30s  %-12s  %-20s\n%!" nom
              (format_duration (t1 -. t0)) result_str;
            match result_str with
            | "Aucun chemin" -> None
            | _ -> Some (nom, print_detail))
          routeurs
      in
      separator ();

      if afficheurs <> [] then begin
        let choix =
          raw_select "\nAfficher les details des chemins ?" [ "Oui"; "Non" ]
        in
        if choix = "Oui" then begin
          Printf.printf "\n== Details des chemins : ==============\n";
          List.iter
            (fun (nom, print_detail) ->
              Printf.printf "-------> %s\n" nom;
              print_detail ();
              separator ())
            afficheurs
        end
      end


module type Random_Bench = sig
  type graph
  type node
  type routeur_pair
  type weight

  (* Créer des paires (A* et Dijkstra) de routeurs *)
  val make_routeur_pairs : graph -> routeur_pair list

  (** Récupérer les nœuds d'un graphe *)
  val get_nodes : graph -> node list

  (** Trouver des points de départ et d'arrivée pour le routage avec le graphe *)
  val build_context : graph -> node -> node -> (graph * node * node) option

  (** Construire le poids initial pour ce scénario de benchmark *)
  val make_start_weight : unit -> weight

  (** Lancer les benchmarks pour une paire de routeurs *)
  val benchmark_pair :
    routeur_pair list -> graph -> node -> node -> weight ->
    (string * int * int * float * float * float * float) option
end

(** Foncteur pour créer un exécuteur de benchmark aléatoire générique *)
module MakeRandomBench (RBench : Random_Bench) = struct

  let nb_bench_alea = 100
  let attempts_limit = nb_bench_alea * 10

  (** Vérifier le nombre de nœuds dans le graphe *)
  let verif_nb_nodes graph =
    let nodes = RBench.get_nodes graph in
    let num_nodes = List.length nodes in
    if num_nodes < 2 then
      failwith "Pas assez de noeuds pour faire de benchmarks aléatoires.";
    (nodes, num_nodes)

  (** Choisir la taille maximale pour chercher parmi les noeuds *)
  let choose_max_size num_nodes =
    Printf.printf "Taille maximale pour chercher parmi les noeuds (taille max : %d) : %!" num_nodes;
    let max_size = 
      try int_of_string (String.trim (read_line ()))
      with _ -> num_nodes
    in min max_size num_nodes


  let run_random_bench_console graph =

    Printf.printf "\n=== Benchmark Mode Aléatoire ===\n%!";
    let (nodes, num_nodes) = verif_nb_nodes graph in

    Printf.printf "Nombre de nœuds disponibles: %d\n%!" num_nodes;
    let max_size = choose_max_size num_nodes in
    Printf.printf "\nDémarrage de %d benchmarks aléatoires \n%!" nb_bench_alea;

    (* Initialisation*)
    Random.self_init ();
    let init = RBench.make_start_weight () in
    let routeur_pairs = RBench.make_routeur_pairs graph in
    let stats = init_category_stats () in
    let filtered_nodes = filter_nodes nodes max_size in

    (* Collection des donnés de bench dans la hash table*)
    let rec collect remaining attempts_limit attempts =
      if (remaining = 0 || attempts_limit <= 0) then
        stats
      else
        let attempts = attempts + 1 in
        if attempts mod (nb_bench_alea / 10) = 0 then
          Printf.printf "Progression: %d tentatives pour %d benchmarks valides \n%!"
            attempts (nb_bench_alea - remaining);

        (* Lancer le benchmark d'une paire de routeurs *)
        let try_benchmark () =
          match pick_random_pair filtered_nodes with
          | None -> None
          | Some (src, dst) -> (match RBench.build_context graph src dst with
              | None -> None
              | Some (graph_for_search, src_found, dst_found) -> RBench.benchmark_pair routeur_pairs graph_for_search src_found dst_found init)
        in

        (* Ajouter le bench si valide dans la hash table *)
        (match try_benchmark () with
        | None -> collect remaining (attempts_limit - 1) attempts
        | Some (category, elapsed_wins, dijkstra_wins, dijkstra_avg, astar_avg, d_avg_w, a_avg_w) ->
            update_category_stats stats category dijkstra_avg astar_avg d_avg_w a_avg_w elapsed_wins dijkstra_wins;
            collect (remaining - 1) (attempts_limit - 1) attempts)
    in
    
    let _ = collect nb_bench_alea attempts_limit 0 in
    print_random_bench_results stats
end