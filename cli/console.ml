open Metrocaml

let rec raw_select prompt options =
  Printf.printf "%s (%s) : %!" prompt (String.concat " / " options);
  let choice = read_line () in
  let choice2 = String.lowercase_ascii (String.trim choice) in
    match List.find_opt (fun s -> String.lowercase_ascii (String.trim s) = choice2) options with
      | Some c -> c 
      | None -> 
          Printf.printf "Choix invalide. Veuillez taper l'une des options exactement.\n%!";
          raw_select prompt options

let run_console graphe search_callback =
  Printf.printf "\n=== Mode Ligne de Commande ===\n%!";

  Printf.printf "Entrez le nom de la station source : %!";
  let source_name = read_line () in

  Printf.printf "Entrez le nom de la station destination : %!";
  let dest_name = read_line () in

  let get_station_id name =
    match Util.find_station_by_name graphe name with
    | Some station -> station.id
    | None -> ""
  in

  let source_id = get_station_id source_name in
  let dest_id = get_station_id dest_name in

  let algo = raw_select "Choisissez l'algorithme : " [ "BFS"; "Dijkstra" ] in

  let poids =
    if algo = "Dijkstra" then begin
      raw_select "Choisissez le poids : " ["Default"; "Temps"; "Distance"; "Arrets"; "Correspondances"; "Rapide"; "Confort"; "Court chemin"; "Equilibre"; "Explorateur"; "Combinaison" ];
    end else ""
  in

  let coeffs =
    if algo = "Dijkstra" && poids = "Combinaison" then
      let rec ask_coeffs () =
        Printf.printf "Entrez les 4 coefficients (Temps Distance Arrêts Correspondances) : %!";
        let input = read_line () in
        let parts = String.split_on_char ' ' input |> List.filter (fun s -> s <> "") in
        
        try
          let floats = List.map float_of_string parts in
          if List.length floats = 4 then 
            floats
          else (
            Printf.printf "Erreur : Il faut exactement 4 valeurs (reçu : %d).\n" (List.length floats);
            ask_coeffs ()
          )
        with Failure _ ->
          Printf.printf "Erreur : Saisie invalide. Veuillez n'entrer que des nombres.\n";
          ask_coeffs ()
      in
      ask_coeffs ()
    else []
  in

  search_callback source_id dest_id algo poids coeffs