open Bogue
open Gui_types
open Gui_draw
open Gui_utils


(* ================================================== *)
(*                       RUN                          *)
(* ================================================== *)
let run noeuds aretes algos poids_list schedule_items on_search =

  (* -------------------- Initialisation  -------------------- *)
  ignore (Tsdl.Sdl.init Tsdl.Sdl.Init.video);

  let screen_w, screen_h =
    match Tsdl.Sdl.get_display_usable_bounds 0 with
    | Ok rect -> (Tsdl.Sdl.Rect.w rect, Tsdl.Sdl.Rect.h rect)
    | Error _ ->
      (match Tsdl.Sdl.get_desktop_display_mode 0 with
       | Ok mode -> (mode.Tsdl.Sdl.dm_w, mode.Tsdl.Sdl.dm_h)
       | Error _ -> (!largeur, !hauteur))
  in

  (* On ouvre au maximum de l'ecran (hors barres systeme). *)
  largeur := max window_min_width (screen_w - window_screen_padding);
  hauteur := max window_min_height (screen_h - window_screen_padding);

  (* Le panneau prend ~30% de la largeur, avec bornes pour petits/grands ecrans. *)
  let panel_width =
    let preferred = !largeur * panel_ratio_percent / 100 in
    max panel_min_width (min panel_max_width preferred)
  in

  canvas_w := max canvas_min_width (!largeur - panel_width);
  canvas_h := !hauteur;

  let bornes = calcule_bornes noeuds in
  let cats = List.sort_uniq String.compare (List.map (fun (a : arete) -> a.categorie) aretes) in

  let widget_dessin = Widget.sdl_area ~w:!canvas_w ~h:!canvas_h () in
  mon_canvas := Some (Widget.get_sdl_area widget_dessin);

  let etat_initial = {
    noeuds; aretes;
    positions = calcule_positions bornes noeuds;
    noeuds_visibles = [];
    aretes_groupees = Hashtbl.create 1;
    noeuds_actifs = Hashtbl.create 1;
    categories = List.map (fun c -> (c, String.lowercase_ascii c <> "bus" && String.lowercase_ascii c <> "transfert")) cats;
    source = None; dest = None; show_names = false;
    algos; algo_actif = List.hd algos;
    poids_list; poids_actif = List.hd poids_list;
    camera_x = 0.;
    camera_y = 0.;
    zoom = 1.0;
  } |> rebuild_render in
  

  let current_etat = ref etat_initial in

  (* ================================================== *)
  (*                ONGLET: REGLAGE/CARTE              *)
  (* ================================================== *)

  let text_toggle nom actif =
    if actif then "[ON] " ^ nom else "[OFF] " ^ nom
  in

  let text_select nom actif =
    if actif then "[*] " ^ nom else "[ ] " ^ nom
  in

  let btn_noms = Widget.button "Noms des stations : OFF" in

  let boutons_categories = List.map (fun (nom, actif) ->
    (nom, Widget.button (text_toggle nom actif))
  ) etat_initial.categories in

  let boutons_algos = List.map (fun nom ->
    (nom, Widget.button (text_select nom (nom = etat_initial.algo_actif)))
  ) etat_initial.algos in

  let boutons_poids = List.map (fun nom ->
    (nom, Widget.button (text_select nom (nom = etat_initial.poids_actif)))
  ) etat_initial.poids_list in

  (* -------------------- Bloc horaires -------------------- *)
  
  let tm = Unix.localtime (Unix.time ()) in
  let curr_heure = ref tm.Unix.tm_hour in
  let curr_minute = ref tm.Unix.tm_min in
  let is_depart = ref true in

  let str_h () = Printf.sprintf "%02d h" !curr_heure in
  let str_m () = Printf.sprintf "%02d m" !curr_minute in

  let btn_dep_arr = Widget.button "DÉPART" in
  let btn_maintenant = Widget.button "MAINTENANT" in
  
  let btn_h_minus = Widget.button "-" in
  let lbl_h = Widget.label (str_h ()) in
  let btn_h_plus = Widget.button "+" in
  
  let btn_m_minus = Widget.button "-" in
  let lbl_m = Widget.label (str_m ()) in
  let btn_m_plus = Widget.button "+" in

  let update_time_display () =
    Widget.set_text lbl_h (str_h ());
    Widget.set_text lbl_m (str_m ())
  in

  (* -------------------- Bloc sliders (poids) -------------------- *)
  let slider_len =
    max panel_slider_min_length (panel_width - panel_slider_width_margin)
  in
  let s_temps = Widget.slider ~length:slider_len 50 in
  let s_dist = Widget.slider ~length:slider_len 50 in
  let s_arrets = Widget.slider ~length:slider_len 50 in
  let s_corresp = Widget.slider ~length:slider_len 50 in

  let fond_slider = Layout.color_bg (rgb_to_bogue (208, 214, 222)) in
  let slider_cell s = Layout.resident ~background:fond_slider s in

  let row_temps = Layout.flat [Layout.resident (Widget.label "Temps :"); slider_cell s_temps] in
  let row_dist = Layout.flat [Layout.resident (Widget.label "Dist. :"); slider_cell s_dist] in
  let row_arrets = Layout.flat [Layout.resident (Widget.label "Arrêts:"); slider_cell s_arrets] in
  let row_corresp = Layout.flat [Layout.resident (Widget.label "Corres:"); slider_cell s_corresp] in

  let box_sliders = Layout.tower [row_temps; row_dist; row_arrets; row_corresp] in

  (* -------------------- Titres et sous-blocs -------------------- *)
  let titre_panneau = Widget.label  "------------------------------CONTROLES------------------------------" in
  let label_cat = Widget.label      "------------------------------CATEGORIES-----------------------------" in
  let label_algo = Widget.label     "-----------------------------ALGORITHMES-----------------------------" in
  let label_poids = Widget.label    "--------------------------------POIDS--------------------------------" in
  let label_horaire = Widget.label  "-------------------------------HORAIRE-------------------------------" in
  let label_sliders = Widget.label  "-----------------------------COMBINAISON-----------------------------" in

  let block_actions = Layout.tower_of_w [btn_noms] in

  (* Layout de l'horaire *)
  let ligne_dep_maint = Layout.flat_of_w [btn_dep_arr; btn_maintenant] in
  let ligne_h = Layout.flat_of_w [btn_h_minus; lbl_h; btn_h_plus] in
  let ligne_m = Layout.flat_of_w [btn_m_minus; lbl_m; btn_m_plus] in
  let ligne_temps = Layout.flat ~align:Draw.Center [ligne_h; Layout.empty ~w:5 ~h:1 (); ligne_m] in
  let block_horaire = Layout.tower ~align:Draw.Center [Layout.resident label_horaire; ligne_dep_maint; ligne_temps] in

  let block_algo = Layout.tower_of_w (label_algo :: List.map snd boutons_algos) in
  let block_poids = Layout.tower_of_w (label_poids :: List.map snd boutons_poids) in
  let block_cat = Layout.tower_of_w (label_cat :: List.map snd boutons_categories) in
  let block_combinaison = Layout.tower [Layout.resident label_sliders; box_sliders] in

  let espace = Layout.empty ~w:1 ~h:12 () in

  let show_poids_init = (etat_initial.algo_actif = "Dijkstra") in
  let show_combinaison_init = show_poids_init && etat_initial.poids_actif = "Combinaison" in
  Layout.set_show block_poids show_poids_init;
  Layout.set_show box_sliders show_combinaison_init;
  Layout.set_show block_combinaison show_combinaison_init;

  (* ================================================== *)
  (*               ONGLET: ITINERAIRE                  *)
  (* ================================================== *)
  
  let label_recherche = Widget.label "Recherche d'itineraire" in
  let label_depart = Widget.label "Depart" in
  let input_depart = Widget.text_input ~prompt:"Ex: Chatelet" () in
  let btn_select_depart = Widget.button "Selection carte" in
  let label_arrivee = Widget.label "Arrivee" in
  let input_arrivee = Widget.text_input ~prompt:"Ex: Bastille" () in
  let btn_select_arrivee = Widget.button "Selection carte" in
  let btn_valider_recherche = Widget.button "Rechercher" in
  let fond_barre_recherche = Layout.color_bg (rgb_to_bogue (218, 223, 230)) in
  let search_input_w =
    max search_input_min_width (panel_width - search_input_width_margin)
  in
  let barre_depart = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_depart in
  let barre_arrivee = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_arrivee in

  let station_name_of_id id =
    match List.find_opt (fun (n : noeud) -> n.id = id) noeuds with
    | Some n -> n.nom
    | None -> id
  in

  let mode_selection_carte = ref `Aucun in

  (* ================================================== *)
  (*                 ONGLET HORRAIRE                    *)
  (* ================================================== *)

  let label_horaires = Widget.label "Recherche d'horraires" in
  let label_ligne_h = Widget.label "Ligne" in
  let label_arret_h = Widget.label "Arret" in
  let input_ligne_h = Widget.text_input ~prompt:"Ex : 3" () in
  let input_arret_h = Widget.text_input ~prompt:"Ex : Republique" () in
  let label_h_info = Widget.label "Saisissez une ligne ou un arret ou les deux" in
  let label_dir = Widget.label "Direction" in
  let label_table = Widget.label "Horraires" in

  let barre_ligne_h = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_ligne_h in
  let barre_arret_h = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_arret_h in

  let horaires_row_w =
    max horaires_row_min_width (panel_width - horaires_row_width_margin)
  in
  let horaires_row_h = horaires_row_height in
  let color_cell_w = horaires_color_cell_width in
  let max_search_results = horaires_max_search_results in

  (* Création des 24 cases de résultats (invisibles par défaut) *)
  let result_keys = Array.make max_search_results None in (* stocke les clés des résultats *)
  let result_buttons = Array.init max_search_results (fun _ -> Widget.button "") in (*les boutons vide au depart*)
  let color_cells = Array.init max_search_results (fun _ -> (* les petit carrée de couleur*)
    Layout.empty ~w:color_cell_w ~h:horaires_row_h ()
  ) in
  (* Chaque ligne de résultat est un Layout combinant une cellule de couleur et un bouton *)
  let result_rows = Array.init max_search_results (fun i ->
    let text_cell_w = max 90 (horaires_row_w - color_cell_w - 4) in
    let btn_cell = Layout.resident ~w:text_cell_w ~h:horaires_row_h result_buttons.(i) in
    let row = Layout.flat ~sep:4 ~align:Draw.Center [color_cells.(i); btn_cell] in
    Layout.set_show row false; (* Caché au démarrage *)
    row
  ) in

  let max_direction_buttons = horaires_max_direction_buttons in
  let max_table_rows = horaires_max_table_rows in

  (* Création des boutons de direction (invisibles par défaut) *)
  let direction_buttons = Array.init max_direction_buttons (fun _ -> Widget.button "") in
  let direction_rows = Array.init max_direction_buttons (fun i ->
    let row = Layout.resident ~w:horaires_row_w ~h:horaires_row_h direction_buttons.(i) in
    Layout.set_show row false;
    row
  ) in

  (* Création des lignes d'horaires (invisibles par défaut) *)
  let table_labels = Array.init max_table_rows (fun _ -> Widget.label "") in
  let bg_table_next = Layout.color_bg (rgb_to_bogue (214, 214, 214)) in
  let table_rows = Array.init max_table_rows (fun i ->
    let row =
      if i = 0 then
        Layout.resident ~w:horaires_row_w ~h:horaires_table_row_height
          ~background:bg_table_next table_labels.(i)
      else Layout.resident ~w:horaires_row_w ~h:horaires_table_row_height table_labels.(i)
    in
    Layout.set_show row false;
    row
  ) in


  let direction_box = Layout.tower (Array.to_list direction_rows) in
  let table_box = Layout.tower (Array.to_list table_rows) in

  (* Variables pour retenir ce qu'on a cliqué *)
  let selected_choices = ref ([] : schedule_item list) in
  let selected_direction_idx = ref 0 in

  let result_box = Layout.tower (Array.to_list result_rows) in
  let result_box_clip =
    Layout.make_clip ~scrollbar:true ~scrollbar_width ~h:horaires_result_clip_height result_box
  in
  let direction_box_clip =
    Layout.make_clip ~scrollbar:true ~scrollbar_width ~h:horaires_direction_clip_height direction_box
  in
  let table_box_clip =
    Layout.make_clip ~scrollbar:true ~scrollbar_width ~h:horaires_table_clip_height table_box
  in

  (* ------suppression des résultats---- *)
  
  let clear_result_rows () =
    for i = 0 to max_search_results - 1 do
      result_keys.(i) <- None;
      Layout.set_background color_cells.(i) None;
      Layout.set_show result_rows.(i) false
    done
  in

  let clear_direction_rows () =
    for i = 0 to max_direction_buttons - 1 do
      Layout.set_show direction_rows.(i) false
    done
  in

  let clear_table_rows () =
    for i = 0 to max_table_rows - 1 do
      Widget.set_text table_labels.(i) "";
      Layout.set_show table_rows.(i) false
    done
  in

  (* ------suppression des résultats---- *)

  (* Fonction pour enlever les doublons de ligne+station dans les résultats de recherche d'horaires *)
  let dedupe_line_station items =
    let resultat = Hashtbl.create 128 in
    List.filter (fun (elt : schedule_item) ->
      let key = normalize elt.mode_name ^ "|" ^ normalize elt.line_name ^ "|" ^ elt.station_id in
      if Hashtbl.mem resultat key then false else (Hashtbl.add resultat key true; true)
    ) items
  in

  let choices_for mode_name line_name station_id =
    List.filter
      (fun (it : schedule_item) ->
        normalize it.mode_name = normalize mode_name
        && normalize it.line_name = normalize line_name
        && it.station_id = station_id)
      schedule_items
  in

  let closest_next_index times =
    let now_s = now_seconds_of_day () in
    let rec find i = function
      | [] -> 0
      | t :: _ when t >= now_s -> i
      | _ :: tl -> find (i + 1) tl
    in
    if times = [] then None else Some (find 0 times)
  in

  
  let set_table_for_choice (choice : schedule_item) =
    Widget.set_text label_table (Printf.sprintf "Horaires - %s|%s - %s - vers %s" choice.mode_name choice.line_name choice.station_name choice.direction);(* let a jour le titre de la categorie*)
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
      Widget.set_text table_labels.(0) "Aucun passage disponible";
      Layout.set_show table_rows.(0) true
    end else begin
      let limited_times = List.filteri (fun i _ -> i < max_table_rows) ordered_times in
      List.iteri (fun i t ->
        Widget.set_text table_labels.(i) (format_seconds t);
        Layout.set_show table_rows.(i) true
      ) limited_times
    end
  in

  (* Fonction pour afficher les directions disponibles pour une ligne+station donnée *)
  let update_direction_buttons () =
    clear_direction_rows ();
    let limited_choices = List.filteri (fun i _ -> i < max_direction_buttons) !selected_choices in(* on garde les premier*)
    List.iteri (fun i choice ->
      let marker = if i = !selected_direction_idx then "[*]" else "[ ]" in
      Widget.set_text direction_buttons.(i) (Printf.sprintf "%s vers %s" marker choice.direction);
      Layout.set_show direction_rows.(i) true
    ) limited_choices
  in


  let open_choice mode_name line_name station_id =
    let choices = choices_for mode_name line_name station_id in
    match choices with
    | [] ->
      selected_choices := [];
      clear_direction_rows ();
      clear_table_rows ();
      Widget.set_text label_h_info "Aucun horaire trouve pour cette selection."
    | _ ->
      selected_choices := choices;
      selected_direction_idx := 0;
      update_direction_buttons ();
      set_table_for_choice (List.hd choices)
  in

  (* Fonction pour afficher les résultats de recherche d'horaires *)
  let set_result_rows entries =
    let limited = List.filteri (fun i _ -> i < max_search_results) entries in (*on garde que les premeier*)
    List.iteri (fun i (elt : schedule_item) ->
      result_keys.(i) <- Some (elt.mode_name, elt.line_name, elt.station_id);
      let r, g, b = hex_to_rgb elt.line_color in
      Layout.set_background color_cells.(i) (Some (Layout.color_bg (rgb_to_bogue (r, g, b)))); (*transforme en couleur bogue*)
      Widget.set_text result_buttons.(i) (Printf.sprintf "%s|%s | %s" elt.mode_name elt.line_name elt.station_name);
      Layout.set_show result_rows.(i) true 
    ) limited
  in

  (* Fonction pour lancer la recherche d'horaires à partir des champs de saisie *)
  let launch_horaire_search () =
    clear_result_rows (); 

    let q_line = normalize (Widget.get_text input_ligne_h) in
    let q_stop = normalize (Widget.get_text input_arret_h) in
    
    if q_line = "" && q_stop = "" then 
      Widget.set_text label_h_info "Saisissez une ligne, un arret, ou les deux."
    else begin
      (* On filtre les horaires selon la ligne et/ou l'arrêt saisis, en supprimant les doublons de ligne+station *)
      let filtered =
        schedule_items
        |> List.filter (fun elt -> Gui_utils.contains_ci elt.line_name q_line && Gui_utils.contains_ci elt.station_name q_stop)
        |> dedupe_line_station
      in
      let count = List.length filtered in (*le nombre de station a affichée*)
      
      if count = 0 then 
        Widget.set_text label_h_info "Aucun resultat."
      else begin
        Widget.set_text label_h_info (Printf.sprintf "%d resultat(s) trouve(s)." count);
        set_result_rows filtered (*on affiche les résultats*)
      end
    end
  in

  let contenu_horaires = Layout.tower ~align:Draw.Center [
    Layout.resident ~w:horaires_row_w label_horaires;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident ~w:horaires_row_w label_ligne_h;
    barre_ligne_h;
    Layout.empty ~w:1 ~h:6 ();
    Layout.resident ~w:horaires_row_w label_arret_h;
    barre_arret_h;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident ~w:horaires_row_w label_h_info;
    Layout.empty ~w:1 ~h:8 ();
    result_box_clip;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident ~w:horaires_row_w label_dir;
    direction_box_clip;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident ~w:horaires_row_w label_table;
    table_box_clip;
  ] in
  
  (* -------------------- Contenu des onglets -------------------- *)

  (* Contenu ITINERAIRE  *)
  let contenu_recherche = Layout.tower ~align:Draw.Center [
    Layout.resident label_recherche;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident label_depart;
    Layout.flat [barre_depart; Layout.resident btn_select_depart];
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident label_arrivee;
    Layout.flat [barre_arrivee; Layout.resident btn_select_arrivee];
    Layout.empty ~w:1 ~h:10 ();
    block_horaire;
    Layout.empty ~w:1 ~h:10 ();
    Layout.resident btn_valider_recherche;
  ] in

  (* Contenu REGLAGE/CARTE *)
  let contenu_itineraire = Layout.tower ~align:Draw.Center [
    Layout.resident titre_panneau;
    espace;
    block_actions;
    espace;
    block_algo;
    espace;
    block_poids;
    Layout.empty ~w:1 ~h:8 ();
    block_combinaison;
    espace;
    block_cat;
  ] in

  (* On cache la recherche au démarrage *)
  Layout.set_show contenu_recherche false;
  Layout.set_show contenu_horaires false;

  (* Les contenus d'onglets se superposent pour conserver la meme position visuelle. *)
  let zone_onglet = Layout.superpose [contenu_itineraire; contenu_recherche; contenu_horaires] in
  let h_zone_onglet = max tab_zone_min_height (!hauteur - tab_zone_height_margin) in
  let zone_onglet_scroll =
    Layout.make_clip ~scrollbar:true ~scrollbar_width ~h:h_zone_onglet zone_onglet
  in

  (* -------------------- Barre d'onglets -------------------- *)

  let tab_itineraire = Widget.button "[ REGLAGE ]" in
  let tab_recherche = Widget.button "[ ITINÉRAIRE ]" in
  let tab_horaires = Widget.button "[ HORAIRES ]" in
  
  let barre_onglets = Layout.flat ~align:Draw.Center [
    Layout.resident ~w:tab_button_width tab_itineraire;
    Layout.resident ~w:tab_button_width tab_recherche;
    Layout.resident ~w:tab_button_width tab_horaires;
  ] in



  (* ================================================== *)
  (*                 LOGIQUE UI / ETAT                 *)
  (* ================================================== *)

  let update_ui_buttons () =
    let etat = !current_etat in
    Widget.set_text btn_noms (if etat.show_names then "Noms des stations : ON" else "Noms des stations : OFF");
    
    List.iter (fun (nom, w) ->
      let actif = cherche nom etat.categories true in
      Widget.set_text w (text_toggle nom actif)
    ) boutons_categories;
    
    List.iter (fun (nom, w) ->
      Widget.set_text w (text_select nom (nom = etat.algo_actif))
    ) boutons_algos;
    
    List.iter (fun (nom, w) ->
      Widget.set_text w (text_select nom (nom = etat.poids_actif))
    ) boutons_poids;
    
    (* Visibilite conditionnelle des sections selon algo + poids *)
    let show_poids = (etat.algo_actif = "Dijkstra") in
    let show_combinaison = show_poids && etat.poids_actif = "Combinaison" in
    Layout.set_show block_poids show_poids;
    Layout.set_show box_sliders show_combinaison;
    Layout.set_show block_combinaison show_combinaison
  in

  (* ================================================== *)
  (*                     CONNEXIONS                     *)
  (* ================================================== *)

  let reset_selection_mode () =
    mode_selection_carte := `Aucun;
    Widget.set_text btn_select_depart "Selection carte";
    Widget.set_text btn_select_arrivee "Selection carte"
  in

  let switch_tab ~show_itineraire ~show_recherche ~show_horaires =
    Layout.set_show contenu_itineraire show_itineraire;
    Layout.set_show contenu_recherche show_recherche;
    Layout.set_show contenu_horaires show_horaires;
    reset_selection_mode ();
    rafraichir_ecran !current_etat
  in

  let begin_map_selection mode =
    mode_selection_carte := mode;
    match mode with
    | `Depart ->
      Widget.set_text btn_select_depart "Cliquez sur la carte...";
      Widget.set_text btn_select_arrivee "Selection carte"
    | `Arrivee ->
      Widget.set_text btn_select_arrivee "Cliquez sur la carte...";
      Widget.set_text btn_select_depart "Selection carte"
    | `Aucun ->
      Widget.set_text btn_select_depart "Selection carte";
      Widget.set_text btn_select_arrivee "Selection carte"
  in

  (* Fonction pour lancer la recherche depuis l'onglet itinéraire ou après sélection sur la carte *)
  let launch_search source dest =
    let depart = !is_depart in
    let time_in_secondes = (!curr_heure * 3600) + (!curr_minute * 60) in
    ignore depart;
    let c_temps = float_of_int (Slider.value (Widget.get_slider s_temps)) /. 100.0 in
    let c_dist = float_of_int (Slider.value (Widget.get_slider s_dist)) /. 100.0 in
    let c_arrets = float_of_int (Slider.value (Widget.get_slider s_arrets)) /. 100.0 in
    let c_corresp = float_of_int (Slider.value (Widget.get_slider s_corresp)) /. 100.0 in
    on_search source dest !current_etat.algo_actif !current_etat.poids_actif [c_temps; c_dist; c_arrets; c_corresp] time_in_secondes
  in

  (* Connexions des Onglets *)
  let conn_tab_iti = Widget.connect tab_itineraire tab_itineraire (fun _ _ _ ->
    switch_tab ~show_itineraire:true ~show_recherche:false ~show_horaires:false
  ) Trigger.buttons_down in

  let conn_tab_rech = Widget.connect tab_recherche tab_recherche (fun _ _ _ ->
    switch_tab ~show_itineraire:false ~show_recherche:true ~show_horaires:false
  ) Trigger.buttons_down in

  let conn_tab_hor = Widget.connect tab_horaires tab_horaires (fun _ _ _ ->
    switch_tab ~show_itineraire:false ~show_recherche:false ~show_horaires:true
  ) Trigger.buttons_down in

  let conn_input_ligne_h = Widget.connect input_ligne_h input_ligne_h (fun _ _ _ -> launch_horaire_search ()) [Trigger.text_input; Trigger.key_up] in
  let conn_input_arret_h = Widget.connect input_arret_h input_arret_h (fun _ _ _ -> launch_horaire_search ()) [Trigger.text_input; Trigger.key_up] in

  let conn_select_depart = Widget.connect btn_select_depart btn_select_depart (fun _ _ _ ->
    begin_map_selection `Depart
  ) Trigger.buttons_down in

  let conn_select_arrivee = Widget.connect btn_select_arrivee btn_select_arrivee (fun _ _ _ ->
    begin_map_selection `Arrivee
  ) Trigger.buttons_down in

  (* Action de recherche depuis l'onglet recherche *)
  let conn_valider_rech = Widget.connect btn_valider_recherche btn_valider_recherche (fun _ _ _ ->
    let raw_dep = String.trim (Widget.get_text input_depart) in
    let raw_arr = String.trim (Widget.get_text input_arrivee) in
    
    let source_opt = if raw_dep = "" then !current_etat.source else Some raw_dep in
    let dest_opt = if raw_arr = "" then !current_etat.dest else Some raw_arr in

    current_etat := { !current_etat with source = source_opt; dest = dest_opt };
    update_ui_buttons ();
    rafraichir_ecran !current_etat;
    match source_opt, dest_opt with
    | Some source, Some dest -> launch_search source dest
    | _ -> Printf.printf "Erreur: point source/destination manquant dans l'interface graphique.\n%!"

    
  ) Trigger.buttons_down in

  (* Connexions Horaire *)
  let conn_dep = Widget.connect btn_dep_arr btn_dep_arr (fun _ _ _ ->
    is_depart := not !is_depart;
    Widget.set_text btn_dep_arr (if !is_depart then "DÉPART" else "ARRIVÉE")
  ) Trigger.buttons_down in

  let conn_maint = Widget.connect btn_maintenant btn_maintenant (fun _ _ _ ->
    let tm = Unix.localtime (Unix.time ()) in
    curr_heure := tm.Unix.tm_hour;
    curr_minute := tm.Unix.tm_min;
    update_time_display ()
  ) Trigger.buttons_down in

  let conn_hm = Widget.connect btn_h_minus btn_h_minus (fun _ _ _ ->
    curr_heure := if !curr_heure = 0 then 23 else !curr_heure - 1; 
    update_time_display ()
  ) Trigger.buttons_down in

  let conn_hp = Widget.connect btn_h_plus btn_h_plus (fun _ _ _ ->
    curr_heure := if !curr_heure = 23 then 0 else !curr_heure + 1; 
    update_time_display ()
  ) Trigger.buttons_down in

  let conn_mm = Widget.connect btn_m_minus btn_m_minus (fun _ _ _ ->
    curr_minute := if !curr_minute = 0 then 59 else !curr_minute - 1; 
    update_time_display ()
  ) Trigger.buttons_down in

  let conn_mp = Widget.connect btn_m_plus btn_m_plus (fun _ _ _ ->
    curr_minute := if !curr_minute = 59 then 0 else !curr_minute + 1; 
    update_time_display ()
  ) Trigger.buttons_down in

  let conn_noms = Widget.connect btn_noms btn_noms (fun _ _ _ ->
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
  ) boutons_categories in

  let conn_algos = List.map (fun (nom, w) ->
    Widget.connect w w (fun _ _ _ ->
      current_etat := { !current_etat with algo_actif = nom };
      update_ui_buttons ();
      rafraichir_ecran !current_etat
    ) Trigger.buttons_down
  ) boutons_algos in

  let conn_poids = List.map (fun (nom, w) ->
    Widget.connect w w (fun _ _ _ ->
      current_etat := { !current_etat with poids_actif = nom };
      update_ui_buttons ();
      rafraichir_ecran !current_etat
    ) Trigger.buttons_down
  ) boutons_poids in
  

  let conn_result_rows =
    Array.to_list (Array.mapi (fun i btn ->
      Widget.connect btn btn (fun _ _ _ ->
        match result_keys.(i) with
        | Some (mode_name, line_name, station_id) ->
          open_choice mode_name line_name station_id;
          Widget.set_text label_h_info (Printf.sprintf "Selection: %s|%s" mode_name line_name)
        | None -> ()
      ) Trigger.buttons_down
    ) result_buttons)
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
    ) direction_buttons)
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

    match Trigger.event_kind ev with

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

    | `Mouse_button_down | `Finger_down ->
      let mx, my = Sdl_area.pointer_pos (get_canvas ()) ev in
      let selection =
        match !mode_selection_carte, get_noeud_clique mx my etat with
        | `Depart, Some id ->
          Widget.set_text input_depart (station_name_of_id id);
          Widget.set_text btn_select_depart "Selection carte";
          mode_selection_carte := `Aucun;
          current_etat := { etat with source = Some id };
          true
        | `Arrivee, Some id ->
          Widget.set_text input_arrivee (station_name_of_id id);
          Widget.set_text btn_select_arrivee "Selection carte";
          mode_selection_carte := `Aucun;
          current_etat := { etat with dest = Some id };
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

    | `Mouse_button_up | `Finger_up ->
      if !is_dragging then begin
         is_dragging := false;
         needs_redraw := true
      end

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
  let connexion_carte = Widget.connect_main widget_dessin widget_dessin boucle liste_ev in

  (* ================================================== *)
  (*               ASSEMBLAGE DE L'INTERFACE           *)
  (* ================================================== *)
  
  let fond_gris = Layout.color_bg (rgb_to_bogue (236, 239, 243)) in

  (* La tour avec tout le panneau de contrôle à gauche *)
  let panneau_gauche = Layout.tower ~background:fond_gris ~align:Draw.Center [
    barre_onglets;
    espace;
    zone_onglet_scroll;
  ] in

  Layout.set_size panneau_gauche ~w:panel_width ~h:!hauteur;

  (* La carte à droite *)
  let layout_carte = Layout.resident ~w:!canvas_w ~h:!canvas_h widget_dessin in
  
  (* L'écran coupé en deux avec un espace entre les deux *)
  let interface= Layout.flat [
    panneau_gauche; 
    layout_carte
  ] in

  Layout.set_size interface ~w:!largeur ~h:!hauteur;

  (* ================================================== *)
  (*                     FENETRE                      *)
  (* ================================================== *)

  (* Fonction pour redimensionner les positions des noeuds lors d'un resize de la fenêtre *)
  let scale_positions ~from_w ~from_h ~to_w ~to_h positions =
    let sx = float_of_int to_w /. float_of_int from_w in (* facteur de mise à l'échelle en x *)
    let sy = float_of_int to_h /. float_of_int from_h in (* facteur de mise à l'échelle en y *)
    Hashtbl.iter (fun id (x, y) ->
      Hashtbl.replace positions id (x *. sx, y *. sy)
    ) positions
  in
  (* Fonction pour réinitialiser l'état de glissement après un zoom ou un resize *)
  let reset_drag_state () =
    is_dragging := false;
    dx_glisse := 0;
    dy_glisse := 0
  in

  (* Lors d'un resize de la fenêtre, on doit redimensionner les positions des noeuds pour qu'ils restent à la bonne place sur la carte *)
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

  Layout.on_resize layout_carte apply_map_resize;

  (* On rassemble toutes les connexions pour les passer à la fenêtre *)
  let connexions = connexion_carte :: conn_tab_iti :: conn_tab_rech :: conn_tab_hor :: conn_select_depart :: conn_select_arrivee :: conn_valider_rech :: conn_input_ligne_h :: conn_input_arret_h :: conn_noms :: conn_dep :: conn_maint :: conn_hm :: conn_hp :: conn_mm :: conn_mp :: (conn_cats @ conn_algos @ conn_poids @ conn_result_rows @ conn_direction_rows) in
  let fenetre = Main.of_layout ~connections:connexions interface in

  (* -------------------- Boucle de rendu -------------------- *)
  apply_map_resize ();
  rafraichir_ecran !current_etat;

  let rafraichissement () =
    if !needs_redraw then begin
      needs_redraw := false;
      rafraichir_ecran !current_etat
    end
  in

  Main.run ~before_display:rafraichissement fenetre;
  clear_label_cache ()