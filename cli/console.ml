open Metrocaml

(** Affiche un prompt et demande à l'utilisateur de choisir parmi une liste d'options.
  @param prompt Le message à afficher pour demander la sélection
  @param options La liste des options possibles (ex: ["BFS"; "Dijkstra"])
  @return La chaîne de caractères choisie parmi les options, ou redemande si l'entrée est invalide **)
let rec raw_select prompt options =
  Printf.printf "%s (%s) : %!" prompt (String.concat " / " options);
  let choice = read_line () in
  let choice2 = String.lowercase_ascii (String.trim choice) in
    match List.find_opt (fun s -> String.lowercase_ascii (String.trim s) = choice2) options with
      | Some c -> c 
      | None -> 
          Printf.printf "Erreur : Choix invalide. Veuillez taper l'une des options exactement.\n%!";
          raw_select prompt options

(** Fonction principale pour le mode console. Elle demande à l'utilisateur les informations nécessaires pour effectuer une recherche de chemin, puis appelle la fonction de callback avec ces informations.
  @param graphe Le graphe de transport à utiliser pour la recherche
  @param search_callback La fonction à appeler avec les paramètres de recherche une fois que l'utilisateur les a fournis **)
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

      (** Affiche un prompt pour demander à l'utilisateur d'entrer 4 coefficients flottants séparés par des espaces, correspondant à Temps, Distance, Arrêts et Correspondances.
          La fonction vérifie que l'utilisateur a entré exactement 4 valeurs et que chacune d'elles est un nombre flottant valide.
          Si l'entrée est invalide, un message d'erreur est affiché et l'utilisateur est invité à réessayer. **)
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
      in ask_coeffs()
    else []
  in

  (** Construit la liste des modes de transport disponibles dans le graphe en parcourant tous les nœuds et leurs connexions,
  puis extrait les modes uniques sous forme de chaînes de caractères. **)
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

    (** Affiche un prompt pour demander à l'utilisateur les modes de transport à exclure,
    en vérifiant que les entrées sont valides par rapport à la liste des modes disponibles.
    L'utilisateur peut entrer plusieurs modes séparés par des espaces, ou laisser vide pour n'exclure aucun mode.
    Si une entrée invalide est détectée, un message d'erreur est affiché et l'utilisateur est invité à réessayer. **)
    let rec ask_modes() =
      Printf.printf "Entrez les modes de transport à exclure (séparés par des espaces) parmi : %s, ou laissez vide pour aucun : %!"
        (String.concat " " modes);
      let input = read_line () in
      let parts = String.split_on_char ' ' input |> List.filter (fun s -> s <> "")
    in

    (** Fonction auxiliaire pour trouver un mode valide dans la liste des modes disponibles.
        @param part La chaîne de caractères à vérifier parmis les modes disponibles
        @return Some mode valide si trouvé, ou None si aucun mode ne correspond **)
    let find_mode part =
      List.find_opt (fun m -> String.lowercase_ascii m = String.lowercase_ascii part) modes
    in
    
    (** Fonction qui pour valide chaque partie de l'entrée de l'utilisateur. Si tous les modes sont valides.
        @param acc L'accumulateur pour les modes valides trouvés jusqu'à présent
        @param parts La liste des entrées utilisateur à valider
        @return Some liste de modes valides si tous sont corrects, ou None si une partie est invalide **)
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

    (** Affiche un prompt pour demander à l'utilisateur l'heure de départ au format HH:MM,
    ou permet de laisser vide pour utiliser l'heure actuelle.
    La fonction vérifie que l'entrée est au format correct et que les valeurs d'heure et de minute sont dans les plages valides.
    Si l'entrée est invalide, un message d'erreur est affiché et l'utilisateur est invité à réessayer. **)
    let rec ask_time() =
      Printf.printf "Entrez l'heure de départ (HH:MM) ou laissez vide pour l'heure actuelle : %!";
      let input = String.trim (read_line ()) in
      if input = "" then
        let tm = Unix.localtime (Unix.time ()) in
        (tm.Unix.tm_hour * 3600) + (tm.Unix.tm_min * 60)
      else
        try
          Scanf.sscanf input "%2d:%2d%!" (fun h m ->
            if h < 0 || h > 23 || m < 0 || m > 59 then (
              Printf.printf "Erreur : Heure hors plage. HH:MM doit être compris entre 00:00 et 23:59.\n%!";
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
        | "Oui" -> true
        | _ -> false
  in

  search_callback source_query dest_query algo poids coeffs excluded_modes start_time pmr_transfer