open Metrocaml

let rec raw_select prompt options =
  Printf.printf "%s (%s) : %!" prompt (String.concat " / " options);
  let choice = read_line () in
  let choice2 = String.lowercase_ascii (String.trim choice) in
    match List.find_opt (fun s -> String.lowercase_ascii (String.trim s) = choice2) options with
      | Some c -> c 
      | None -> 
          Printf.printf "Erreur : Choix invalide. Veuillez taper l'une des options exactement.\n%!";
          raw_select prompt options

let run_console graphe search_callback =
  Printf.printf "\n=== Mode Ligne de Commande ===\n%!";

  Printf.printf "Entrez la station source (nom/ID) ou les coordonnées (lat,lon) : %!";
  let source_query = String.trim (read_line ()) in

  Printf.printf "Entrez la station destination (nom/ID) ou les coordonnées (lat,lon) : %!";
  let dest_query = String.trim (read_line ()) in

  let algo = raw_select "Choisissez l'algorithme : " [ "BFS"; "Dijkstra" ] in

  let poids =
    if algo = "Dijkstra" then begin
      raw_select "Choisissez le poids : " ["Default"; "Combinaison" ];
    end else ""
  in

  let coeffs =
    if algo = "Dijkstra" && poids = "Combinaison" then
      let rec ask_coeffs() =
        Printf.printf "Entrez les 4 coefficients (flottants) (Temps Distance Arrêts Correspondances) : %!";
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
          ask_coeffs()
      in
      ask_coeffs()
    else []
  in

  let modes =
    let all_nodes = Network.TransportGraph.nodes graphe in    
    let all_modes_str =
      List.fold_left (fun acc node ->
        let neighbors = Network.TransportGraph.neighbors graphe node in
        List.fold_left (fun acc' (_, connection) ->
          let mode_str = Network.string_of_mode connection.Network.mode in
          mode_str :: acc'
        ) acc neighbors
      ) [] all_nodes
    in
    List.sort_uniq String.compare all_modes_str
  in
  let excluded_modes =
  let rec ask_modes() =
    Printf.printf "Entrez les modes de transport à exclure (séparés par des espaces) parmi : %s, ou laissez vide pour aucun : %!"
      (String.concat " " modes);
    let input = read_line () in
    let parts = String.split_on_char ' ' input |> List.filter (fun s -> s <> "") in
    
    let find_mode part =
      List.find_opt (fun m -> String.lowercase_ascii m = String.lowercase_ascii part) modes
    in
    
    let rec validate_parts acc = function
      | [] -> Some (List.rev acc)
      | hd :: tl ->
          match find_mode hd with
          | Some valid_mode -> validate_parts (valid_mode :: acc) tl
          | None -> None
    in
    
    match validate_parts [] parts with
    | Some valid_modes -> valid_modes
    | None ->
        Printf.printf "Erreur : Modes invalides. Veuillez n'entrer que les modes listés.\n%!";
        ask_modes()
    in ask_modes() 
  in

  let start_time =
  let rec ask_time() =
    Printf.printf "Entrez l'heure de départ (HH:MM) ou laissez vide pour l'heure actuelle : %!";
    let input = String.trim (read_line ()) in
    if input = "" then
      let tm = Unix.localtime (Unix.time ()) in
      (tm.Unix.tm_hour * 3600) + (tm.Unix.tm_min * 60)
    else
      try
        Scanf.sscanf input "%2d:%2d%!" (fun h m ->
          if h < 0 || h > 23 || m < 0 || m > 59 || (h = 24 && m <> 0) then (
            Printf.printf "Erreur : Heure hors plage. H doit être 00..23 et M 00..59 (23:59 max).\n%!";
            ask_time ();)
          else
            h * 3600 + m * 60
          )
      with _ ->
        Printf.printf "Erreur : Format invalide. Veuillez entrer l'heure au format HH:MM.\n%!";
        ask_time()          
    in ask_time() 
  in

  let pmr_transfer = raw_select "Activer PMR : " [ "Oui"; "Non" ] in
  let pmr_transfer =
      match pmr_transfer with
      | "Oui" -> Util.is_pmr
      | _ -> fun s -> true
  in

  search_callback source_query dest_query algo poids coeffs excluded_modes start_time pmr_transfer