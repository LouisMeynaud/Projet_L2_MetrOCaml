open Metrocaml
open Weight
open Routing

let today_yyyymmdd () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

let dir_exists path =
  try (Unix.stat path).Unix.st_kind = Unix.S_DIR with _ -> false

let local_city_exists ville =
  dir_exists ("data/" ^ ville) || dir_exists ("metrocaml/data/" ^ ville)

let choose_city loading_mode = 
    let town = match loading_mode with
    | "En ligne" -> Cli.Console.raw_select "\nChoisissez la ville : " [ "Toulouse"; "Montpellier"; "Marseille"; "Ile-de-France"; "SNCF"; "Corsica"]
    | "Local" -> 
      Cli.Console.raw_select "\nChoisissez la ville : "
      (List.filter (fun ville -> local_city_exists ville)
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
  in Logger.black "\n=== Lancement de MetroCaml ===\n%!";
  Logger.black "Date GTFS cible: %s\n%!" date_cible;
  (date_cible)

let rt_alertes town loading_mode date_cible graph = 
    let (rt_url_alertes) = 
    match town with
    | "Ile-de-France" -> 
        ( "https://prim.iledefrance-mobilites.fr/marketplace/disruptions_bulk/disruptions/v2")
    | _ -> ("") 
  in
  if (rt_url_alertes <> "" && loading_mode == "En ligne" ) then begin
    Printf.printf "\n[Système] Démarrage du moteur Temps Réel en arrière-plan...\n%!";
    Gtfs_rt.start_auto_refresh date_cible graph;
  end; ()

  let affichage_interface graphe =
    Printf.printf "\nChoisissez l'interface à lancer :\n%!";
    Printf.printf "  1 - Interface Graphique (GUI)\n%!";
    Printf.printf "  2 - Interface Ligne de Commande (CLI)\n%!";

    match Cli.Console.raw_select "" [ "1"; "2" ] with
    | "2" ->
      let cli_callback src dest algo poids coeffs excluded_modes start_time pmr_transfer =
      let results = Runner.search_callback src dest algo poids coeffs excluded_modes start_time pmr_transfer graphe true in
      List.iter (fun (nom, details, _) -> Printf.printf "%s\n%s\n%!" nom details) results
    in
      Cli.Console.run_console graphe cli_callback; ()
    | _ -> Logger.black "\nLancement de l'interface graphique...\n%!";
    let gui_callback src dest algo poids coeffs excluded_modes start_time pmr_transfer =
      Runner.search_callback src dest algo poids coeffs excluded_modes start_time pmr_transfer graphe false
    in
      Gui.Transport_view.affiche graphe gui_callback; ()


let () =
  Printf.printf "\x1b[1;33m%s\x1b[0m" "
                   __                                  __
   ____ ___  ___  / /__________  _________ _____ ___  / /
  / __ `__ \\/ _ \\/ __/ ___/ __ \\/ ___/ __ `/ __ `__ \\/ / 
 / / / / / /  __/ /_/ /  / /_/ / /__/ /_/ / / / / / / /  
/_/ /_/ /_/\\___/\\__/_/   \\____/\\___/\\__,_/_/ /_/ /_/_/
";
  let loading_mode = choose_load_mode () in
  let (town, url) = choose_city loading_mode in
  let date_cible = date_choose loading_mode in
  let graph = Runner.load_gtfs_data town url date_cible loading_mode in
  rt_alertes town loading_mode date_cible graph;
  Logger.black "\nGraphe chargé avec succès !\n%!";
  let benchmark =
    match Cli.Console.raw_select "\nVoulez-vous activer le mode benchmark ?" [ "Oui"; "Non" ]  with
    |"Oui" -> true
    |_ -> false
  in
  if benchmark then
    Runner.run_bench_console graph
  else
    affichage_interface graph

  
