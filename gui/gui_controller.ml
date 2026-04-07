open Bogue
open Gui_types
open Gui_draw
open Gui_utils
open Gui_view


(** Initialise et connecte toute la logique de l'interface graphique.
    
    Cette fonction agit comme le Contrôleur (Controller) de l'application. Elle lie 
    les éléments visuels (View) aux données dynamiques (State) et définit les comportements 
    suite aux interactions de l'utilisateur (clics, clavier, souris).

    @param noeuds La liste des nœuds (stations) du graphe.
    @param schedule_items Les données des horaires de transport.
    @param on_search La fonction de callback appelée pour lancer un calcul de plus court chemin. Retourne les détails du chemin sous forme de liste de strings.
    @param current_etat Une référence vers l'état global et muable de l'interface.
    @param view L'enregistrement contenant tous les widgets interactifs générés par [Gui_view].
    @param layout_carte Le composant [Layout.t] représentant la zone de dessin (canvas) de la carte.
    
    @return Un triplet contenant :
    - [connections] : La liste de toutes les connexions d'événements (boutons, molette, etc.) requise par Bogue.
    - [apply_map_resize] : Une fonction à appeler lors du redimensionnement de la fenêtre pour ajuster l'échelle de la carte.
    - [before_display] : Une fonction de boucle (callback) à fournir à [Bogue.Main.run] pour gérer les rafraîchissements d'écran demandés.
*)
let setup ~noeuds ~schedule_items ~on_search ~current_etat ~view ~layout_carte =

	(* Retourne tous les IDs dont le nom correspond exactement (insensible a la casse/espaces) *)
	let exact_ids_for_name name =
		let q = normalize name in
		noeuds
		|> List.filter (fun (n : noeud) -> normalize n.nom = q)
		|> List.map (fun (n : noeud) -> n.id)
	in

	let name_for_id id =
    match List.find_opt (fun (n : noeud) -> n.id = id) noeuds with
    | Some n -> n.nom
    | None -> id
	in

	(* Retourne l'ID si [raw] est deja un ID valide de station. *)
	let station_id_if_valid raw =
		let q = String.trim raw in
		match List.find_opt (fun (n : noeud) -> n.id = q) noeuds with
		| Some n -> Some n.id
		| None -> None
	in

	(* Mode de sélection sur la carte (départ, arrivée, ou aucun) *)
	let mode_selection_carte = ref `Aucun in
	let selected_depart_id = ref !current_etat.source in
	let selected_arrivee_id = ref !current_etat.dest in

	let max_search_results = Array.length view.result_rows in
	let max_direction_buttons = Array.length view.direction_rows in
	let max_table_rows = Array.length view.table_rows in
	let max_route_depart_suggestions = Array.length view.route_depart_rows in
	let max_route_arrivee_suggestions = Array.length view.route_arrivee_rows in

	let found_paths = ref [] in

	(* ================================================== *)
	(*            LOGIQUE HORAIRES / RESULTATS           *)
	(* ================================================== *)
	
	(* Variables pour retenir ce qu'on a cliqué *)
	let selected_choices = ref ([] : schedule_item list) in
	let selected_direction_idx = ref 0 in

	(* ------suppression des résultats---- *)

	let clear_result_rows () =
		for i = 0 to max_search_results - 1 do
			view.result_keys.(i) <- None;
			Layout.set_background view.color_cells.(i) None;
			Layout.set_show view.result_rows.(i) false
		done
	in

	let clear_direction_rows () =
		for i = 0 to max_direction_buttons - 1 do
			Layout.set_show view.direction_rows.(i) false
		done
	in

	let clear_table_rows () =
		for i = 0 to max_table_rows - 1 do
			Widget.set_text view.table_labels.(i) "";
			Layout.set_show view.table_rows.(i) false
		done
	in


	(*enleve les doublons de ligne+station dans les résultats de recherche d'horaires
	@param items Liste d'éléments de type schedule_item
	@return Liste d'éléments sans doublons *)
	let dedupe_line_station items =
		let resultat = Hashtbl.create 128 in
		List.filter (fun (elt : schedule_item) ->
			let key = normalize elt.mode_name ^ "|" ^ normalize elt.line_name ^ "|" ^ elt.station_id in
			if Hashtbl.mem resultat key then false else (Hashtbl.add resultat key true; true)
		) items
	in

	(*trouve les horaires correspondant à une ligne, une station et un mode de transport donnés 
	@param mode_name Nom du mode de transport
	@param line_name Nom de la ligne
	@param station_id ID de la station
	@return Liste d'éléments de type schedule_item *)
	let choices_for mode_name line_name station_id =
		List.filter
			(fun (it : schedule_item) ->
				normalize it.mode_name = normalize mode_name
				&& normalize it.line_name = normalize line_name
				&& it.station_id = station_id)
			schedule_items
	in

	(*trouverl'index du prochain horaire à venir dans une liste d'horaires triée 
	@param times Liste d'horaires triée
	@return Index du prochain horaire à venir *)
	let closest_next_index times =
		let now_s = now_seconds_of_day () in
		let rec find i = function
			| [] -> 0
			| t :: _ when t >= now_s -> i
			| _ :: tl -> find (i + 1) tl
		in
		if times = [] then None else Some (find 0 times)
	in

	(*affiche les horaires d'une ligne+station+direction sélectionnée 
	@param choice élément de type schedule_item *)
	let set_table_for_choice (choice : schedule_item) =
		Widget.set_text view.label_schedule_table (Printf.sprintf "Horaires - %s|%s - %s - vers %s" choice.mode_name choice.line_name choice.station_name choice.direction);
		clear_table_rows ();

		let times = List.sort_uniq compare choice.times in
		let ordered_times =
			match closest_next_index times with
			| None -> []
			| Some idx ->
					let futures = List.filteri (fun i _ -> i >= idx) times in
					let passees = List.filteri (fun i _ -> i < idx) times in
					futures @ passees
		in

		(*selectionne les 12 premier horraire et les affiches*)
		if ordered_times = [] then begin
			Widget.set_text view.table_labels.(0) "Aucun passage disponible";
			Layout.set_show view.table_rows.(0) true
		end else begin
			let limited_times = List.filteri (fun i _ -> i < max_table_rows) ordered_times in
			List.iteri (fun i t ->
				Widget.set_text view.table_labels.(i) (format_seconds t);
				Layout.set_show view.table_rows.(i) true
			) limited_times
		end
	in

	(* affiche les directions disponibles pour une ligne+station donnée 
	@param choices Liste des choix de direction *)
	let update_direction_buttons () =
		clear_direction_rows ();
		let limited_choices = List.filteri (fun i _ -> i < max_direction_buttons) !selected_choices in(* on garde les premier*)
		List.iteri (fun i choice ->
			let marker = if i = !selected_direction_idx then "[*]" else "[ ]" in
			Widget.set_text view.direction_buttons.(i) (Printf.sprintf "%s vers %s" marker choice.direction);
			Layout.set_show view.direction_rows.(i) true
		) limited_choices
	in

	(*ouvre le choix d'horaires à partir des résultats de recherche 
	@param mode_name Nom du mode de transport
	@param line_name Nom de la ligne
	@param station_id ID de la station *)
	let open_choice mode_name line_name station_id =
		let choices = choices_for mode_name line_name station_id in
		match choices with
		| [] ->
			selected_choices := [];
			clear_direction_rows ();
			clear_table_rows ();
			Widget.set_text view.label_schedule_info "Aucun horaire trouve pour cette selection."
		| _ ->
			selected_choices := choices;
			selected_direction_idx := 0;
			update_direction_buttons ();
			set_table_for_choice (List.hd choices)
	in

	(*affiche les résultats de recherche d'horaires 
	@param entries Liste des résultats de recherche *)
	let set_result_rows entries =
		let limited = List.filteri (fun i _ -> i < max_search_results) entries in (*on garde que les premeier*)
		List.iteri (fun i (elt : schedule_item) ->
			view.result_keys.(i) <- Some (elt.mode_name, elt.line_name, elt.station_id);
			let r, g, b = hex_to_rgb elt.line_color in
			Layout.set_background view.color_cells.(i) (Some (Layout.color_bg (rgb_to_bogue (r, g, b)))); (*transforme en couleur bogue*)
			Widget.set_text view.result_buttons.(i) (Printf.sprintf "%s|%s | %s" elt.mode_name elt.line_name elt.station_name);
			Layout.set_show view.result_rows.(i) true
		) limited
	in

	(*lance la recherche d'horaires à partir des champs de saisie *)
	let launch_horaire_search () =
		clear_result_rows ();

		let q_line = normalize (Widget.get_text view.input_schedule_line) in
		let q_stop = normalize (Widget.get_text view.input_schedule_stop) in

		if q_line = "" && q_stop = "" then
			Widget.set_text view.label_schedule_info "Saisissez une ligne, un arret, ou les deux."
		else begin
			(* On filtre les horaires selon la ligne et/ou l'arrêt saisis, en supprimant les doublons de ligne+station *)
			let filtered =
				schedule_items
				|> List.filter (fun elt -> Gui_utils.contains_ci elt.line_name q_line && Gui_utils.contains_ci elt.station_name q_stop)
				|> dedupe_line_station
			in
			let count = List.length filtered in (*le nombre de station a affichée*)

			if count = 0 then
				Widget.set_text view.label_schedule_info "Aucun resultat."
			else begin
				Widget.set_text view.label_schedule_info (Printf.sprintf "%d resultat(s) trouve(s)." count);
				set_result_rows filtered (*on affiche les résultats*)
			end
		end
	in

	(* ================================================== *)
	(*                 LOGIQUE UI / ETAT                 *)
	(* ================================================== *)

    let clear_suggestions keys rows max_val =
        for i = 0 to max_val - 1 do
            keys.(i) <- None;
            Layout.set_show rows.(i) false
        done
    in

    let update_suggestions text keys buttons rows max_val =
        clear_suggestions keys rows max_val;
        let q = normalize text in
        if q <> "" then begin
            let rec find_matches acc count = function
                | [] -> List.rev acc
                | _ when count >= max_val -> List.rev acc
                | n :: tl ->
                    if Gui_utils.contains_ci n.nom q then
                        find_matches (n :: acc) (count + 1) tl
                    else
                        find_matches acc count tl
            in
            let matches = find_matches [] 0 noeuds in

            List.iteri (fun i (n : noeud) ->
                keys.(i) <- Some n.id;
                Widget.set_text buttons.(i) (Printf.sprintf "%s (%s)" n.nom n.id);
                Layout.set_show rows.(i) true
            ) matches
        end
    in

    let show_route_depart_suggestions text =
        update_suggestions text view.route_depart_keys view.route_depart_buttons view.route_depart_rows max_route_depart_suggestions
    in

    let show_route_arrivee_suggestions text =
        update_suggestions text view.route_arrivee_keys view.route_arrivee_buttons view.route_arrivee_rows max_route_arrivee_suggestions
    in

	let clear_route_depart_suggestions () =
		clear_suggestions view.route_depart_keys view.route_depart_rows max_route_depart_suggestions
	in

	let clear_route_arrivee_suggestions () =
		clear_suggestions view.route_arrivee_keys view.route_arrivee_rows max_route_arrivee_suggestions
	in

	let set_depart_id id =
		selected_depart_id := Some id;
		current_etat := { !current_etat with source = Some id }
	in

	let set_arrivee_id id =
		selected_arrivee_id := Some id;
		current_etat := { !current_etat with dest = Some id }
	in

	(* Met à jour les textes et la visibilité des boutons en fonction de l'état actuel *)
	let update_ui_buttons () =
		let etat = !current_etat in
		Widget.set_text view.button_station_names (if etat.show_names then "Noms des stations : ON" else "Noms des stations : OFF");

		List.iter (fun (nom, w) ->
			let actif = cherche nom etat.categories true in
			Widget.set_text w (text_toggle nom actif)
		) view.category_buttons;

		List.iter (fun (nom, w) ->
			Widget.set_text w (text_select nom (nom = etat.algo_actif))
		) view.algorithm_buttons;

		List.iter (fun (nom, w) ->
			Widget.set_text w (text_select nom (nom = etat.poids_actif))
		) view.weight_buttons;

		(* Visibilite conditionnelle des sections selon algo + poids *)
		let show_poids = (etat.algo_actif = "Dijkstra") in
		let show_combinaison = show_poids && etat.poids_actif = "Combinaison" in
		Layout.set_show view.weights_block show_poids;
		Layout.set_show view.sliders_box show_combinaison;
		Layout.set_show view.combination_block show_combinaison
	in

	(* ================================================== *)
	(*                     CONNEXIONS                     *)
	(* ================================================== *)

	(* Réinitialise le mode de sélection sur la carte et les textes des boutons associés *)
	let reset_selection_mode () =
		mode_selection_carte := `Aucun;
		Widget.set_text view.button_select_departure_on_map "Selection carte";
		Widget.set_text view.button_select_arrival_on_map "Selection carte"
	in

	(* Fonction pour basculer entre les onglets Itinéraire, Recherche, Horaires et Résultats 
	@param show_itineraire Booléen pour afficher l'onglet Itinéraire
	@param show_recherche Booléen pour afficher l'onglet Recherche
	@param show_horaires Booléen pour afficher l'onglet Horaires
	@param show_results Booléen pour afficher l'onglet Résultats
	*)
	let switch_tab ~show_itineraire ~show_recherche ~show_horaires ~show_results =
		Layout.set_show view.tab_content_settings show_itineraire;
		Layout.set_show view.tab_content_route show_recherche;
		Layout.set_show view.tab_content_schedules show_horaires;
		Layout.set_show view.tab_content_results show_results;
		reset_selection_mode ()
	in

	(*change les textes des boutons de sélection sur la carte
	@param mode Mode de sélection *)
	let begin_map_selection mode =
		mode_selection_carte := mode;
		match mode with
		| `Depart ->
			Widget.set_text view.button_select_departure_on_map "Cliquez sur la carte...";
			Widget.set_text view.button_select_arrival_on_map "Selection carte"
		| `Arrivee ->
			Widget.set_text view.button_select_arrival_on_map "Cliquez sur la carte...";
			Widget.set_text view.button_select_departure_on_map "Selection carte"
		| `Aucun ->
			Widget.set_text view.button_select_departure_on_map "Selection carte";
			Widget.set_text view.button_select_arrival_on_map "Selection carte"
	in

	(* Lance la recherche d'itinéraire avec les paramètres actuels et les champs de saisie 
	@param source Point de départ
	@param dest Point d'arrivée
	*)
	let launch_search source dest =
		let depart = !(view.is_departure_mode) in
		let time_in_secondes = (!(view.current_hour) * 3600) + (!(view.current_minute) * 60) in
		ignore depart;
		let excluded_modes =
			!current_etat.categories
			|> List.filter (fun (mode_name, is_enabled) ->
				not is_enabled && not (String.lowercase_ascii mode_name = "transfert"))
			|> List.map fst
		in
		let c_temps = float_of_int (Slider.value (Widget.get_slider view.slider_time)) /. 100.0 in
		let c_dist = float_of_int (Slider.value (Widget.get_slider view.slider_distance)) /. 100.0 in
		let c_arrets = float_of_int (Slider.value (Widget.get_slider view.slider_stops)) /. 100.0 in
		let c_corresp = float_of_int (Slider.value (Widget.get_slider view.slider_transfers)) /. 100.0 in
		on_search source dest !current_etat.algo_actif !current_etat.poids_actif [c_temps; c_dist; c_arrets; c_corresp] excluded_modes time_in_secondes
	in

	(* ---------connexions onglets et boutons--------- *)
	let conn_tab_iti = Widget.connect view.tab_settings view.tab_settings (fun _ _ _ ->
		switch_tab ~show_itineraire:true ~show_recherche:false ~show_horaires:false ~show_results:false
	) Trigger.buttons_down in

	let conn_tab_rech = Widget.connect view.tab_route view.tab_route (fun _ _ _ ->
		switch_tab ~show_itineraire:false ~show_recherche:true ~show_horaires:false ~show_results:false
	) Trigger.buttons_down in

	let conn_tab_hor = Widget.connect view.tab_schedules view.tab_schedules (fun _ _ _ ->
		switch_tab ~show_itineraire:false ~show_recherche:false ~show_horaires:true ~show_results:false
	) Trigger.buttons_down in

	let conn_tab_res = Widget.connect view.tab_results view.tab_results (fun _ _ _ ->
		switch_tab ~show_itineraire:false ~show_recherche:false ~show_horaires:false ~show_results:true
	) Trigger.buttons_down in


	let conn_input_details = Widget.connect view.input_result_details view.input_result_details (fun _ _ _ ->
		let query = String.trim (Widget.get_text view.input_result_details) in
		let selection =
			try
				let idx = int_of_string query - 1 in
				if idx >= 0 && idx < List.length !found_paths then
					Some (List.nth !found_paths idx)
				else None
			with _ ->
				List.find_opt (fun (nom, _, _) -> nom = query) !found_paths
		in
		match selection with
		| Some (_, details, edges_opt) ->
			Widget.set_text view.result_display details;
			current_etat := { !current_etat with highlighted_edges = match edges_opt with Some e -> e | None -> [] };
			rafraichir_ecran !current_etat;
			Widget.set_text view.input_result_details ""
		| None -> ()
	) [Trigger.text_input; Trigger.key_up] in

	(*ecoute les saisie clavier dans ligne_horaire et arret_horaire pour mettre a jour les proposition*)
	let conn_input_ligne_h = Widget.connect view.input_schedule_line view.input_schedule_line (fun _ _ _ -> launch_horaire_search ()) [Trigger.text_input; Trigger.key_up] in
	let conn_input_arret_h = Widget.connect view.input_schedule_stop view.input_schedule_stop (fun _ _ _ -> launch_horaire_search ()) [Trigger.text_input; Trigger.key_up] in

	let conn_input_depart = Widget.connect view.input_depart view.input_depart (fun _ _ _ ->
		selected_depart_id := None;
		current_etat := { !current_etat with
			source = None; 
			highlighted_edges = []
			};
		rafraichir_ecran !current_etat;
		show_route_depart_suggestions (Widget.get_text view.input_depart)
	) [Trigger.text_input; Trigger.key_up] in

	let conn_input_arrivee = Widget.connect view.input_arrivee view.input_arrivee (fun _ _ _ ->
		selected_arrivee_id := None;
		current_etat := { !current_etat with
			dest = None; 
			highlighted_edges = [] 
			};
		rafraichir_ecran !current_etat;
		show_route_arrivee_suggestions (Widget.get_text view.input_arrivee)
	) [Trigger.text_input; Trigger.key_up] in

	(* Boutons de sélection sur la carte pour le départ et l'arrivée *)
	let conn_select_depart = Widget.connect view.button_select_departure_on_map view.button_select_departure_on_map (fun _ _ _ ->
		begin_map_selection `Depart
	) Trigger.buttons_down in

	(* Connexion pour le bouton de sélection d'arrivée sur la carte *)
	let conn_select_arrivee = Widget.connect view.button_select_arrival_on_map view.button_select_arrival_on_map (fun _ _ _ ->
		begin_map_selection `Arrivee
	) Trigger.buttons_down in

	(* Connexion pour le bouton de validation de la recherche d'itinéraire *)
	let conn_valider_rech = Widget.connect view.button_validate_search view.button_validate_search (fun _ _ _ ->
		let raw_dep = String.trim (Widget.get_text view.input_depart) in
		let raw_arr = String.trim (Widget.get_text view.input_arrivee) in
		let source_opt =
			match !selected_depart_id with
			| Some id -> Some id
			| None ->
				(match station_id_if_valid raw_dep with
				| Some id ->
					Widget.set_text view.input_depart (name_for_id id);
					set_depart_id id;
					clear_route_depart_suggestions ();
					Some id
				| None ->
					(match exact_ids_for_name raw_dep with
					| [id] ->
						Widget.set_text view.input_depart (name_for_id id);
						set_depart_id id;
						clear_route_depart_suggestions ();
						Some id
					| [] -> None
					| _ ->
						show_route_depart_suggestions raw_dep;
						Printf.printf "Plusieurs stations portent ce nom pour le depart. Choisissez une suggestion.\n%!";
						None))
		in
		let dest_opt =
			match !selected_arrivee_id with
			| Some id -> Some id
			| None ->
				(match station_id_if_valid raw_arr with
				| Some id ->
					Widget.set_text view.input_arrivee (name_for_id id);
					set_arrivee_id id;
					clear_route_arrivee_suggestions ();
					Some id
				| None ->
					(match exact_ids_for_name raw_arr with
					| [id] ->
						Widget.set_text view.input_arrivee (name_for_id id);
						set_arrivee_id id;
						clear_route_arrivee_suggestions ();
						Some id
					| [] -> None
					| _ ->
						show_route_arrivee_suggestions raw_arr;
						Printf.printf "Plusieurs stations portent ce nom pour l'arrivee. Choisissez une suggestion.\n%!";
						None))
		in

		current_etat := { !current_etat with source = source_opt; dest = dest_opt };
		update_ui_buttons ();
		rafraichir_ecran !current_etat;
		let source_query_opt =
			match source_opt with
			| Some source -> Some source
			| None -> if raw_dep = "" then None else Some raw_dep
		in
		let dest_query_opt =
			match dest_opt with
			| Some dest -> Some dest
			| None -> if raw_arr = "" then None else Some raw_arr
		in
		match source_query_opt, dest_query_opt with
		| Some source, Some dest -> let results = launch_search source dest in
			found_paths := results;
			let last_edges = List.fold_left (fun acc (_, _, edges_opt) -> match edges_opt with Some e -> Some e | None -> acc) None (List.rev results) in
			current_etat := { !current_etat with highlighted_edges = match last_edges with Some e -> e | None -> [] };
			let summary = "Chemins disponibles:\n" ^ String.concat "\n" (List.map (fun (nom, _, _) -> nom) results) in
			Widget.set_text view.result_display summary;
			switch_tab ~show_itineraire:false ~show_recherche:false ~show_horaires:false ~show_results:true
		| _ -> Printf.printf "Erreur: point source/destination manquant dans l'interface graphique.\n%!"


	) Trigger.buttons_down in

	(*--------- connexions horraire-------------*)
	
	(* Connexion pour le bouton de basculement entre mode départ/arrivée *)
	let conn_dep = Widget.connect view.button_departure_arrival view.button_departure_arrival (fun _ _ _ ->
		view.is_departure_mode := not !(view.is_departure_mode);
		Widget.set_text view.button_departure_arrival (if !(view.is_departure_mode) then "DÉPART" else "ARRIVÉE")
	) Trigger.buttons_down in

	(* Connexion pour le bouton de mise à l'heure actuelle *)
	let conn_maint = Widget.connect view.button_now view.button_now (fun _ _ _ ->
		let tm = Unix.localtime (Unix.time ()) in
		view.current_hour := tm.Unix.tm_hour;
		view.current_minute := tm.Unix.tm_min;
		view.update_time_display ()
	) Trigger.buttons_down in

	let conn_hm = Widget.connect view.button_hour_minus view.button_hour_minus (fun _ _ _ ->
		view.current_hour := if !(view.current_hour) = 0 then 23 else !(view.current_hour) - 1;
		view.update_time_display ()
	) Trigger.buttons_down in

	let conn_hp = Widget.connect view.button_hour_plus view.button_hour_plus (fun _ _ _ ->
		view.current_hour := if !(view.current_hour) = 23 then 0 else !(view.current_hour) + 1;
		view.update_time_display ()
	) Trigger.buttons_down in

	let conn_mm = Widget.connect view.button_minute_minus view.button_minute_minus (fun _ _ _ ->
		view.current_minute := if !(view.current_minute) = 0 then 59 else !(view.current_minute) - 1;
		view.update_time_display ()
	) Trigger.buttons_down in

	let conn_mp = Widget.connect view.button_minute_plus view.button_minute_plus (fun _ _ _ ->
		view.current_minute := if !(view.current_minute) = 59 then 0 else !(view.current_minute) + 1;
		view.update_time_display ()
	) Trigger.buttons_down in



	(* Connexion pour l'affichage des noms de stations *)
	let conn_noms = Widget.connect view.button_station_names view.button_station_names (fun _ _ _ ->
		current_etat := { !current_etat with show_names = not !current_etat.show_names };
		update_ui_buttons ();
		rafraichir_ecran !current_etat
	) Trigger.buttons_down in

	let conn_cats = List.map (fun (nom, w) ->
		Widget.connect w w (fun _ _ _ ->
			let new_cats = List.map (fun (c, v) -> if c = nom then (c, not v) else (c, v)) !current_etat.categories in
			current_etat := rebuild_render{ !current_etat with categories = new_cats };
			update_ui_buttons ();
			rafraichir_ecran !current_etat
		) Trigger.buttons_down
	) view.category_buttons in

	let conn_algos = List.map (fun (nom, w) ->
		Widget.connect w w (fun _ _ _ ->
			current_etat := { !current_etat with algo_actif = nom };
			update_ui_buttons ()
		) Trigger.buttons_down
	) view.algorithm_buttons in

	let conn_poids = List.map (fun (nom, w) ->
		Widget.connect w w (fun _ _ _ ->
			current_etat := { !current_etat with poids_actif = nom };
			update_ui_buttons ()
		) Trigger.buttons_down
	) view.weight_buttons in


	let conn_result_rows =
		Array.to_list (Array.mapi (fun i btn ->
			Widget.connect btn btn (fun _ _ _ ->
				match view.result_keys.(i) with
				| Some (mode_name, line_name, station_id) ->
					open_choice mode_name line_name station_id;
					Widget.set_text view.label_schedule_info (Printf.sprintf "Selection: %s|%s" mode_name line_name)
				| None -> ()
			) Trigger.buttons_down
		) view.result_buttons)
	in

	let conn_direction_rows =
		Array.to_list (Array.mapi (fun i btn ->
			Widget.connect btn btn (fun _ _ _ ->
				match List.nth_opt !selected_choices i with
				| Some choice ->
					selected_direction_idx := i;
					update_direction_buttons ();
					set_table_for_choice choice
				| None -> ()
			) Trigger.buttons_down
		) view.direction_buttons)
	in

	let conn_route_depart_rows =
		Array.to_list (Array.mapi (fun i btn ->
			Widget.connect btn btn (fun _ _ _ ->
				match view.route_depart_keys.(i) with
				| Some id ->
					set_depart_id id;
					Widget.set_text view.input_depart (name_for_id id);
					clear_route_depart_suggestions ()
				| None -> ()
			) Trigger.buttons_down
		) view.route_depart_buttons)
	in

	let conn_route_arrivee_rows =
		Array.to_list (Array.mapi (fun i btn ->
			Widget.connect btn btn (fun _ _ _ ->
				match view.route_arrivee_keys.(i) with
				| Some id ->
					set_arrivee_id id;
					Widget.set_text view.input_arrivee (name_for_id id);
					clear_route_arrivee_suggestions ()
				| None -> ()
			) Trigger.buttons_down
		) view.route_arrivee_buttons)
	in

	(* ================================================== *)
	(*                 EVENEMENTS CARTE                  *)
	(* ================================================== *)

	let is_dragging = ref false in
	let debut_x = ref 0 in
	let debut_y = ref 0 in
	let dx_glisse = ref 0 in
	let dy_glisse = ref 0 in
	let start_camera_x = ref 0.0 in
	let start_camera_y = ref 0.0 in
	let needs_redraw = ref false in

	let boucle _w _target ev =
		let etat = !current_etat in

		(*ecoute en continue les evenements*)
		match Trigger.event_kind ev with

		(*ecoute la molette de la souris pour le zoom*)
		| `Mouse_wheel ->
			let wheel_delta = Tsdl.Sdl.Event.(get ev mouse_wheel_y) in
			if wheel_delta <> 0 then begin
				let etat = !current_etat in
				let facteur = if wheel_delta > 0 then zoom_in_factor else zoom_out_factor in
				let next_zoom = etat.zoom *. facteur in
				if next_zoom >= min_zoom && next_zoom <= max_zoom then begin
					let mx, my = Sdl_area.pointer_pos (get_canvas ()) ev in
					let mx_f = float mx and my_f = float my in
					let world_x = (mx_f -. etat.camera_x) /. etat.zoom in
					let world_y = (my_f -. etat.camera_y) /. etat.zoom in
					let new_camera_x = mx_f -. world_x *. next_zoom in
					let new_camera_y = my_f -. world_y *. next_zoom in
					current_etat := { etat with zoom = next_zoom; camera_x = new_camera_x; camera_y = new_camera_y };
					needs_redraw := true
				end
			end

		(*ecoute le clique pour la sélection sur la carte*)
		| `Mouse_button_down | `Finger_down ->
			let mx, my = Sdl_area.pointer_pos (get_canvas ()) ev in
			let selection =
				match !mode_selection_carte, get_noeud_clique mx my etat with
				| `Depart, Some id ->
					Widget.set_text view.input_depart (name_for_id id);
					clear_route_depart_suggestions ();
					Widget.set_text view.button_select_departure_on_map "Selection carte";
					mode_selection_carte := `Aucun;
					set_depart_id id;
					true
				| `Arrivee, Some id ->
					Widget.set_text view.input_arrivee(name_for_id id);
					clear_route_arrivee_suggestions ();
					Widget.set_text view.button_select_arrival_on_map "Selection carte";
					mode_selection_carte := `Aucun;
					set_arrivee_id id;
					true
				| (`Depart | `Arrivee), None -> false
				| `Aucun, _ -> false
			in

			if not selection then begin
				is_dragging := true;
				debut_x := mx;
				debut_y := my;
				dx_glisse := 0;
				dy_glisse := 0;
				start_camera_x := etat.camera_x;
				start_camera_y := etat.camera_y
			end else begin
				update_ui_buttons ();
				needs_redraw := true
			end

		(*ecoute le relachement de la souris*)
		| `Mouse_button_up | `Finger_up ->
			if !is_dragging then begin
				 is_dragging := false;
				 needs_redraw := true
			end

		(*ecoute le mouvement de la souris pour faire glisser la carte si on est en train de glisser*)
		| `Mouse_motion | `Finger_motion ->
			if !is_dragging then begin
				let mx, my = Sdl_area.pointer_pos (get_canvas ()) ev in
				dx_glisse := mx - !debut_x;
				dy_glisse := my - !debut_y;
				current_etat := {
					etat with
					camera_x = !start_camera_x +. float !dx_glisse;
					camera_y = !start_camera_y +. float !dy_glisse;
				};
				needs_redraw := true
			end
		| _ -> ()
	in

	let liste_ev = Trigger.buttons_down @ Trigger.buttons_up @ Trigger.pointer_motion @ [Tsdl.Sdl.Event.mouse_wheel] in
	let connexion_carte = Widget.connect_main view.widget_canvas view.widget_canvas boucle liste_ev in

	(* ================================================== *)
	(*                     RESIZE                        *)
	(* ================================================== *)

	(* Met à l'échelle les positions des nœuds en fonction du redimensionnement de la fenetre *)
	let scale_positions ~from_w ~from_h ~to_w ~to_h positions =
		let sx = float_of_int to_w /. float_of_int from_w in (* facteur de mise à l'échelle en x *)
		let sy = float_of_int to_h /. float_of_int from_h in (* facteur de mise à l'échelle en y *)
		Hashtbl.iter (fun id (x, y) ->
			Hashtbl.replace positions id (x *. sx, y *. sy)
		) positions
	in

	let reset_drag_state () =
		is_dragging := false;
		dx_glisse := 0;
		dy_glisse := 0
	in

	(*detecte si la fenetre est redomensionnée , si oui , met a jour les position et redessine*)
	let apply_map_resize () =
		let old_w = max 1 !canvas_w in
		let old_h = max 1 !canvas_h in
		let new_w, new_h = Layout.get_physical_size layout_carte in
		let size_changed =
			new_w > max_resize_safety_threshold
			&& new_h > max_resize_safety_threshold
			&& (new_w <> old_w || new_h <> old_h)
		in
		if size_changed then begin
			canvas_w := new_w;
			canvas_h := new_h;
			scale_positions ~from_w:old_w ~from_h:old_h ~to_w:new_w ~to_h:new_h !current_etat.positions;
			let sx = float_of_int new_w /. float_of_int old_w in
			let sy = float_of_int new_h /. float_of_int old_h in
			current_etat := {
				!current_etat with
				camera_x = !current_etat.camera_x *. sx;
				camera_y = !current_etat.camera_y *. sy;
			};
			reset_drag_state ();
			needs_redraw := true
		end
	in

	let connections = connexion_carte :: conn_tab_iti :: conn_tab_rech :: conn_tab_hor :: conn_tab_res :: conn_select_depart :: conn_select_arrivee :: conn_valider_rech :: conn_input_ligne_h :: conn_input_arret_h :: conn_input_details :: conn_input_depart :: conn_input_arrivee :: conn_noms :: conn_dep :: conn_maint :: conn_hm :: conn_hp :: conn_mm :: conn_mp :: (conn_cats @ conn_algos @ conn_poids @ conn_result_rows @ conn_direction_rows @ conn_route_depart_rows @ conn_route_arrivee_rows) in

	let before_display () =
		if !needs_redraw then begin
			needs_redraw := false;
			rafraichir_ecran !current_etat
		end
	in

	(connections, apply_map_resize, before_display)
