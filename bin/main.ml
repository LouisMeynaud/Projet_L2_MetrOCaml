open Metrocaml
open Weight
open Routing

let today_yyyymmdd () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday


let choose_city () = let town = Cli.Console.raw_select "Choisissez la ville : " [ "Toulouse"; "Montpellier"; "Marseille"; "Ile-de-France" ] in
  let url =
    match town with
    | "Toulouse" -> "https://www.data.gouv.fr/api/1/datasets/r/64318177-f5b5-4144-a6be-2d0f22d26c77"
    | "Montpellier" -> "https://www.data.gouv.fr/api/1/datasets/r/85ef3b33-2289-4d19-b479-276e5b1c5a52"
    | "Marseille" -> "https://www.data.gouv.fr/api/1/datasets/r/ab931edc-0bc1-4c66-960a-dc546b46df03"
    | "Ile-de-France" -> "https://eu.ftp.opendatasoft.com/stif/GTFS/IDFM-gtfs.zip"
    | _ -> failwith "Ville inconnue"
  in (town, url)


let date_choose = let date_cible = today_yyyymmdd ()
  in Printf.printf "=== Lancement de MetroCaml ===\n%!";
  Printf.printf "Date GTFS cible: %s\n%!" date_cible;
  (date_cible)


let affichage_interface graphe =
    Printf.printf "Choisissez l'interface à lancer :\n%!";
    Printf.printf "  1 - Interface Graphique (GUI)\n%!";
    Printf.printf "  2 - Interface Ligne de Commande (CLI)\n%!";

    match Cli.Console.raw_select "" [ "1"; "2" ] with
    | "2" -> 
        let cli_callback src dest algo poids coeffs =
          Runner.search_callback src dest algo poids coeffs (8 * 3600) graphe false
        in
        Cli.Console.run_console graphe cli_callback

    | _ ->
        Printf.printf "Lancement de l'interface graphique...\n%!";
        let gui_callback src dest algo poids coeffs start_time =
          Runner.search_callback src dest algo poids coeffs start_time graphe false
        in
        Gui.Transport_view.affiche graphe gui_callback; ()


let () =
  let (town, url) = choose_city () in
  let date_cible = date_choose in
  let graph = Runner.load_gtfs_data town url date_cible in
  Printf.printf "\nGraphe chargé avec succès !\n%!";
  affichage_interface graph;

  
