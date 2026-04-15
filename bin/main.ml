open Metrocaml
open Weight
open Routing

let today_yyyymmdd () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday


let choose_city loading_mode = 
    let town = match loading_mode with
    | "En ligne" -> Cli.Console.raw_select "Choisissez la ville : " [ "Toulouse"; "Montpellier"; "Marseille"; "Ile-de-France"; "SNCF"; "Corsica"]
    | "Local" -> 
      Cli.Console.raw_select "Choisissez la ville : "
      (List.filter(fun ville -> try (Unix.stat ("data/" ^ ville )).Unix.st_kind = Unix.S_DIR (*Vérifie si le dossier existe et si c'est bien un dossier (S_DIR)*)
                                with _ -> false (*Plutôt que de planter on retourne false.*))
      [ "Toulouse"; "Montpellier"; "Marseille"; "Ile-de-France"; "SNCF"; "Corsica"])
    | _ -> failwith "Mode de chargement inconnu"
  in
  let url =
    match town with
    | "Toulouse" -> "https://www.data.gouv.fr/api/1/datasets/r/64318177-f5b5-4144-a6be-2d0f22d26c77"
    | "Montpellier" -> "https://data.montpellier3m.fr/sites/default/files/ressources/TAM_MMM_GTFS.zip"
    | "Marseille" -> "https://www.data.gouv.fr/api/1/datasets/r/ab931edc-0bc1-4c66-960a-dc546b46df03"
    | "Ile-de-France" -> "https://eu.ftp.opendatasoft.com/stif/GTFS/IDFM-gtfs.zip"
    | "SNCF" -> "https://www.data.gouv.fr/api/1/datasets/r/9ae758ec-cd7a-40cd-a890-bb3963224942"
    | "Corsica" -> "https://www.data.gouv.fr/api/1/datasets/r/b95d6838-1143-4653-840f-eebc33c16fa7"
    | _ -> failwith "Ville inconnue"
  in (town, url)

let choose_load_mode () = let loading_mode = Cli.Console.raw_select "\nChoisissez le mode de chargement : " [ "En ligne"; "Local"] in
  loading_mode


let date_choose loading_mode = let date_cible =
  match loading_mode with
  | "En ligne" -> today_yyyymmdd ()
  | "Local" -> "20260330"
  | _ -> failwith "Mode de chargement inconnu"
  in Printf.printf "=== Lancement de MetroCaml ===\n%!";
  Printf.printf "Date GTFS cible: %s\n%!" date_cible;
  (date_cible)

 
(* Fonctions pour l'affichag *)
let node_to_string (s : Network.station) = s.name
let edge_to_string (c : Network.connection) = c.line.name
let edge_to_color (c : Network.connection) = c.line.color
let edge_to_duration (c : Network.connection) = c.duration
let edge_to_distance (c : Network.connection) = c.distance
 
(* Construit un routeur pour les tests de bench *)
let make_routeur nom (module Algo : Runner.ALGO) =
  let fn graph source destination start_time =
    match Algo.find_path ~start_time ~graph ~source ~destination ~transfer_allowed:(fun _ -> true) with
    | None -> ("Aucun chemin", fun () -> ())
    | Some p -> let module ShowPath = Show.Make (Algo.P) in
    let temps, _ = (ShowPath.calcul_temps_distance edge_to_duration edge_to_distance p.steps) in
    let h, m = (temps / 3600) , (temps / 60 mod 60) in
    let result_str = Printf.sprintf "OK - %d etape(s) %d heures %d minutes" (List.length p.Algo.P.steps) h m  in
      let print () =
        ShowPath.print_path
          node_to_string
          edge_to_string
          Algo.P.Weight.to_string
          edge_to_color
          edge_to_duration
          edge_to_distance
          false
          true
          p
      in
      (result_str, print)
  in
  (nom, fn)
 
let build_bench_routeurs () =
  let preset_routeurs =
    List.map (fun (preset : Presets.preset) ->
      let module W = (val Presets.to_weight preset) in
      let module D = Dijkstra.Make (Network.TransportGraph) (W) in
      make_routeur
        (Printf.sprintf "Preset : %s" preset.name)
        (module struct
          module P = D.P
          let find_path = D.find_path
        end)
    ) Presets.all
  in
  let unit_routeurs = [
    make_routeur "Poids : Temps"
      (module struct
        module P = Runner.DIJKSTRA_TimeWeight.P
        let find_path = Runner.DIJKSTRA_TimeWeight.find_path
      end);
    make_routeur "Poids :  Distance"
      (module struct
        module P = Runner.DIJKSTRA_DistanceWeight.P
        let find_path = Runner.DIJKSTRA_DistanceWeight.find_path
      end);
    make_routeur "Poids : Arrets"
      (module struct
        module P = Runner.DIJKSTRA_HopWeight.P
        let find_path = Runner.DIJKSTRA_HopWeight.find_path
      end);
    make_routeur "Poids : Correspondances"
      (module struct
        module P = Runner.DIJKSTRA_CorrespondenceWeight.P
        let find_path = Runner.DIJKSTRA_CorrespondenceWeight.find_path
      end);
    make_routeur "Arrets (BFS) : "
      (module struct
        module P = Runner.BFS_HopWeight.P
        let find_path ~start_time ~graph ~source ~destination ~transfer_allowed:_ =
          Runner.BFS_HopWeight.find_path ~start_time ~graph ~source ~destination
      end);
  ] in
  preset_routeurs @ unit_routeurs
 
let run_bench_console graphe =
  Printf.printf "\n Benchmark activé \n%!";
  let routeurs = build_bench_routeurs () in
    Printf.printf "Station source : %!";
    let source_query = String.trim (read_line ()) in
    Printf.printf "Station destination : %!";
    let dest_query = String.trim (read_line ()) in
    let tm = Unix.localtime (Unix.time ()) in
    let start_time = tm.Unix.tm_hour * 3600 + tm.Unix.tm_min * 60 in
    Bench.run graphe source_query dest_query start_time routeurs Cli.Console.raw_select


  let affichage_interface graphe =
    Printf.printf "Choisissez l'interface à lancer :\n%!";
    Printf.printf "  1 - Interface Graphique (GUI)\n%!";
    Printf.printf "  2 - Interface Ligne de Commande (CLI)\n%!";

    match Cli.Console.raw_select "" [ "1"; "2" ] with
    | "2" -> 
  let cli_callback src dest algo poids coeffs excluded_modes start_time =
    let results = Runner.search_callback src dest algo poids coeffs excluded_modes start_time false graphe false true in
      List.iter (fun (nom, details, _) -> Printf.printf "%s:\n%s\n\n%!" nom details) results
    in
      Cli.Console.run_console graphe cli_callback; ()
    | _ -> Printf.printf "Lancement de l'interface graphique...\n%!";
    let gui_callback src dest algo poids coeffs excluded_modes start_time pmr_transfer =
      Runner.search_callback src dest algo poids coeffs excluded_modes start_time pmr_transfer graphe false false
    in
      Gui.Transport_view.affiche graphe gui_callback; ()


let () =
  let loading_mode = choose_load_mode () in
  let (town, url) = choose_city loading_mode in
  let date_cible = date_choose loading_mode in
  let graph = Runner.load_gtfs_data town url date_cible loading_mode in
  Printf.printf "\nGraphe chargé avec succès !\n%!";
  let benchmark =
    match Cli.Console.raw_select "\nVoulez-vous activer le mode benchmark ?" [ "Oui"; "Non" ]  with
    |"Oui" -> true
    |_ -> false
  in
  if benchmark then
    run_bench_console graph
  else
    affichage_interface graph

  
