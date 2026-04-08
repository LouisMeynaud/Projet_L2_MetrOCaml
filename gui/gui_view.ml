open Bogue
open Gui_types
open Gui_draw

let text_toggle name is_active =
  if is_active then "[ON] " ^ name else "[OFF] " ^ name

let text_select name is_active =
  if is_active then "[*] " ^ name else "[ ] " ^ name

(** Type pour représenter les éléments de l'interface graphique. *)
type t = {
  widget_canvas : Widget.t;
  button_station_names : Widget.t;
  category_buttons : (string * Widget.t) list;
  algorithm_buttons : (string * Widget.t) list;
  weight_buttons : (string * Widget.t) list;
  current_hour : int ref;
  current_minute : int ref;
  is_departure_mode : bool ref;
  button_departure_arrival : Widget.t;
  button_now : Widget.t;
  button_hour_minus : Widget.t;
  label_hour : Widget.t;
  button_hour_plus : Widget.t;
  button_minute_minus : Widget.t;
  label_minute : Widget.t;
  button_minute_plus : Widget.t;
  update_time_display : unit -> unit;
  slider_time : Widget.t;
  slider_distance : Widget.t;
  slider_stops : Widget.t;
  slider_transfers : Widget.t;
  weights_block : Layout.t;
  sliders_box : Layout.t;
  combination_block : Layout.t;
  input_depart : Widget.t;
  input_arrivee : Widget.t;
  route_depart_keys : string option array;
  route_depart_buttons : Widget.t array;
  route_depart_rows : Layout.t array;
  route_arrivee_keys : string option array;
  route_arrivee_buttons : Widget.t array;
  route_arrivee_rows : Layout.t array;
  button_select_departure_on_map : Widget.t;
  button_select_arrival_on_map : Widget.t;
  button_pmr_transfers : Widget.t;
  button_validate_search : Widget.t;
  input_schedule_line : Widget.t;
  input_schedule_stop : Widget.t;
  label_schedule_info : Widget.t;
  label_schedule_table : Widget.t;
  input_result_details : Widget.t;
  result_display : Widget.t;
  result_keys : (string * string * string) option array;
  result_buttons : Widget.t array;
  color_cells : Layout.t array;
  result_rows : Layout.t array;
  direction_buttons : Widget.t array;
  direction_rows : Layout.t array;
  table_labels : Widget.t array;
  table_rows : Layout.t array;
  tab_content_settings : Layout.t;
  tab_content_route : Layout.t;
  tab_content_schedules : Layout.t;
  tab_content_results : Layout.t;
  tab_settings : Widget.t;
  tab_route : Widget.t;
  tab_schedules : Widget.t;
  tab_results : Widget.t;
  tabs_bar : Layout.t;
  tabs_content_scroll : Layout.t;
  spacer : Layout.t;
  left_panel : Layout.t;
  layout_carte : Layout.t;
  interface : Layout.t;
}

let build ~panel_width ~(etat_initial : etat) =
  (* ================================================== *)
  (*                CANVAS / BASE UI                   *)
  (* ================================================== *)
  let widget_canvas = Widget.sdl_area ~w:!canvas_w ~h:!canvas_h () in
  mon_canvas := Some (Widget.get_sdl_area widget_canvas);

  (* ================================================== *)
  (*                 ONGLET REGLAGE                    *)
  (* ================================================== *)

  (* -------------------- Partie actions -------------------- *)
  let button_station_names = Widget.button "Noms des stations : OFF" in

  (* -------------------- Partie algorithmes/categorie/poids -------------------- *)
  let category_buttons =
    List.map (fun (name, is_active) ->
      (name, Widget.button (text_toggle name is_active))) etat_initial.categories
  in

  let algorithm_buttons =
    List.map (fun name ->
      (name, Widget.button (text_select name (name = etat_initial.algo_actif)))) etat_initial.algos
  in

  let weight_buttons =
    List.map (fun name ->
      (name, Widget.button (text_select name (name = etat_initial.poids_actif)))) etat_initial.poids_list
  in

  (* -------------------- Etat horaire (utilise par l'onglet Itineraire) -------------------- *)
  let tm = Unix.localtime (Unix.time ()) in
  let current_hour = ref tm.Unix.tm_hour in
  let current_minute = ref tm.Unix.tm_min in
  let is_departure_mode = ref true in

  let str_h () = Printf.sprintf "%02d h" !current_hour in
  let str_m () = Printf.sprintf "%02d m" !current_minute in

  let button_departure_arrival = Widget.button "DÉPART" in
  let button_now = Widget.button "MAINTENANT" in
  let button_hour_minus = Widget.button "-" in
  let label_hour = Widget.label (str_h ()) in
  let button_hour_plus = Widget.button "+" in
  let button_minute_minus = Widget.button "-" in
  let label_minute = Widget.label (str_m ()) in
  let button_minute_plus = Widget.button "+" in

  let update_time_display () =
    Widget.set_text label_hour (str_h ());
    Widget.set_text label_minute (str_m ())
  in

  (* -------------------- Partie combinaison (sliders) -------------------- *)
  let slider_len = max panel_slider_min_length (panel_width - panel_slider_width_margin) in
  let slider_time = Widget.slider ~length:slider_len 50 in
  let slider_distance = Widget.slider ~length:slider_len 50 in
  let slider_stops = Widget.slider ~length:slider_len 50 in
  let slider_transfers = Widget.slider ~length:slider_len 50 in

  let fond_slider = Layout.color_bg (rgb_to_bogue (208, 214, 222)) in
  let slider_cell s = Layout.resident ~background:fond_slider s in

  let row_temps = Layout.flat [Layout.resident (Widget.label "Temps :"); slider_cell slider_time] in
  let row_dist = Layout.flat [Layout.resident (Widget.label "Dist. :"); slider_cell slider_distance] in
  let row_arrets = Layout.flat [Layout.resident (Widget.label "Arrêts:"); slider_cell slider_stops] in
  let row_corresp = Layout.flat [Layout.resident (Widget.label "Corres:"); slider_cell slider_transfers] in

  let sliders_box = Layout.tower [row_temps; row_dist; row_arrets; row_corresp] in

  let titre_panneau = Widget.label "------------------------------CONTROLES------------------------------" in
  let label_cat = Widget.label "------------------------------CATEGORIES-----------------------------" in
  let label_algo = Widget.label "-----------------------------ALGORITHMES-----------------------------" in
  let label_poids = Widget.label "--------------------------------POIDS--------------------------------" in
  let label_horaire = Widget.label "-------------------------------HORAIRE-------------------------------" in
  let label_sliders = Widget.label "-----------------------------COMBINAISON-----------------------------" in

  let block_actions = Layout.tower_of_w [button_station_names] in

  let block_algo = Layout.tower_of_w (label_algo :: List.map snd algorithm_buttons) in
  let weights_block = Layout.tower_of_w (label_poids :: List.map snd weight_buttons) in
  let block_cat = Layout.tower_of_w (label_cat :: List.map snd category_buttons) in
  let combination_block = Layout.tower [Layout.resident label_sliders; sliders_box] in

  let spacer = Layout.empty ~w:1 ~h:12 () in

  let show_poids_init = etat_initial.algo_actif = "Dijkstra" in
  let show_combinaison_init = show_poids_init && etat_initial.poids_actif = "Combinaison" in
  Layout.set_show weights_block show_poids_init;
  Layout.set_show sliders_box show_combinaison_init;
  Layout.set_show combination_block show_combinaison_init;

  (* ================================================== *)
  (*               ONGLET ITINERAIRE                   *)
  (* ================================================== *)

  let label_recherche = Widget.label "Recherche d'itineraire" in
  let label_depart = Widget.label "Depart" in
  let input_depart = Widget.text_input ~prompt:"Ex: Chatelet" () in
  let button_select_departure_on_map = Widget.button "Selection carte" in
  let label_arrivee = Widget.label "Arrivee" in
  let input_arrivee = Widget.text_input ~prompt:"Ex: Bastille" () in
  let button_select_arrival_on_map = Widget.button "Selection carte" in
  
  let ligne_dep_maint = Layout.flat_of_w [button_departure_arrival; button_now] in
  let ligne_h = Layout.flat_of_w [button_hour_minus; label_hour; button_hour_plus] in
  let ligne_m = Layout.flat_of_w [button_minute_minus; label_minute; button_minute_plus] in
  let ligne_temps = Layout.flat ~align:Draw.Center [ligne_h; Layout.empty ~w:5 ~h:1 (); ligne_m] in
  let block_horaire = Layout.tower ~align:Draw.Center [Layout.resident label_horaire; ligne_dep_maint; ligne_temps] in
  let button_pmr_transfers =
    Widget.button
      (text_select "Correspondance PMR uniquement"
         etat_initial.pmr_transfer)
  in
  let button_validate_search = Widget.button "Rechercher" in

  let fond_barre_recherche = Layout.color_bg (rgb_to_bogue (218, 223, 230)) in
  let search_input_w = max search_input_min_width (panel_width - search_input_width_margin) in
  let barre_depart = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_depart in
  let barre_arrivee = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_arrivee in

  let route_depart_keys = Array.make route_max_suggestions None in
  let route_depart_buttons = Array.init route_max_suggestions (fun _ -> Widget.button "") in
  let route_depart_rows =
    Array.init route_max_suggestions (fun i ->
      let row = Layout.resident ~w:search_input_w ~h:route_suggestion_row_height route_depart_buttons.(i) in
      Layout.set_show row false;
      row)
  in
  let route_depart_box = Layout.tower (Array.to_list route_depart_rows) in
  let route_depart_clip =
    Layout.make_clip ~scrollbar:true ~scrollbar_width ~h:route_suggestion_clip_height route_depart_box
  in

  let route_arrivee_keys = Array.make route_max_suggestions None in
  let route_arrivee_buttons = Array.init route_max_suggestions (fun _ -> Widget.button "") in
  let route_arrivee_rows =
    Array.init route_max_suggestions (fun i ->
      let row = Layout.resident ~w:search_input_w ~h:route_suggestion_row_height route_arrivee_buttons.(i) in
      Layout.set_show row false;
      row)
  in
  let route_arrivee_box = Layout.tower (Array.to_list route_arrivee_rows) in
  let route_arrivee_clip =
    Layout.make_clip ~scrollbar:true ~scrollbar_width ~h:route_suggestion_clip_height route_arrivee_box
  in

  (* ================================================== *)
  (*                 ONGLET HORAIRES                   *)
  (* ================================================== *)

  (* -------------------- Partie recherche -------------------- *)
  let label_horaires = Widget.label "Recherche d'horraires" in
  let label_ligne_h = Widget.label "Ligne" in
  let label_arret_h = Widget.label "Arret" in
  let input_schedule_line = Widget.text_input ~prompt:"Ex : 3" () in
  let input_schedule_stop = Widget.text_input ~prompt:"Ex : Republique" () in
  let label_schedule_info = Widget.label "Saisissez une ligne ou un arret ou les deux" in
  let label_dir = Widget.label "Direction" in
  let label_schedule_table = Widget.label "Horraires" in

  let barre_ligne_h = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_schedule_line in
  let barre_arret_h = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_schedule_stop in

  let horaires_row_w = max horaires_row_min_width (panel_width - horaires_row_width_margin) in
  let horaires_row_h = horaires_row_height in
  let color_cell_w = horaires_color_cell_width in

  (* -------------------- Partie listes de resultats -------------------- *)
  let result_keys = Array.make horaires_max_search_results None in
  let result_buttons = Array.init horaires_max_search_results (fun _ -> Widget.button "") in
  let color_cells = Array.init horaires_max_search_results (fun _ ->
    Layout.empty ~w:color_cell_w ~h:horaires_row_h ())
  in

  let result_rows = Array.init horaires_max_search_results (fun i ->
    let text_cell_w = max 90 (horaires_row_w - color_cell_w - 4) in
    let btn_cell = Layout.resident ~w:text_cell_w ~h:horaires_row_h result_buttons.(i) in
    let row = Layout.flat ~sep:4 ~align:Draw.Center [color_cells.(i); btn_cell] in
    Layout.set_show row false;
    row)
  in

  let direction_buttons = Array.init horaires_max_direction_buttons (fun _ -> Widget.button "") in
  let direction_rows = Array.init horaires_max_direction_buttons (fun i ->
    let row = Layout.resident ~w:horaires_row_w ~h:horaires_row_h direction_buttons.(i) in
    Layout.set_show row false;
    row)
  in

  let table_labels = Array.init horaires_max_table_rows (fun _ -> Widget.label "") in
  let bg_table_next = Layout.color_bg (rgb_to_bogue (214, 214, 214)) in
  let table_rows = Array.init horaires_max_table_rows (fun i ->
    let row =
      if i = 0 then
        Layout.resident ~w:horaires_row_w ~h:horaires_table_row_height ~background:bg_table_next table_labels.(i)
      else Layout.resident ~w:horaires_row_w ~h:horaires_table_row_height table_labels.(i)
    in
    Layout.set_show row false;
    row)
  in

  let direction_box = Layout.tower (Array.to_list direction_rows) in
  let table_box = Layout.tower (Array.to_list table_rows) in
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

  (* -------------------- Contenu des onglets -------------------- *)
  let tab_content_schedules = Layout.tower ~align:Draw.Center [
    Layout.resident ~w:horaires_row_w label_horaires;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident ~w:horaires_row_w label_ligne_h;
    barre_ligne_h;
    Layout.empty ~w:1 ~h:6 ();
    Layout.resident ~w:horaires_row_w label_arret_h;
    barre_arret_h;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident ~w:horaires_row_w label_schedule_info;
    Layout.empty ~w:1 ~h:8 ();
    result_box_clip;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident ~w:horaires_row_w label_dir;
    direction_box_clip;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident ~w:horaires_row_w label_schedule_table;
    table_box_clip;
  ] in

  (* ================================================== *)
  (*                 ONGLET RESULTATS                   *)
  (* ================================================== *)

  let label_resultats = Widget.label "Resultats de recherche" in
  let input_result_details = Widget.text_input ~prompt:"Details du chemin..." () in
  let barre_details = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_result_details in

  (* Zone pour afficher les resultats verticalement *)
  let result_display = Widget.text_display "" in
  let resultats_w = max resultats_min_width (panel_width - resultats_width_margin) in
  let resultats_row_width = max resultats_row_min_width (panel_width - resultats_row_width_margin) in
  let result_layout = Layout.resident ~w:resultats_w ~h:resultats_height result_display in

  let tab_content_results = Layout.tower ~align:Draw.Center [
    Layout.resident label_resultats;
    Layout.empty ~w:resultats_row_width ~h:resultats_row_height ();
    barre_details;
    Layout.empty ~w:resultats_row_width ~h:resultats_row_height ();
    result_layout;
  ] in

  let tab_content_route = Layout.tower ~align:Draw.Center [
    Layout.resident label_recherche;
    Layout.empty ~w:resultats_row_width ~h:resultats_row_height ();
    Layout.resident label_depart;
    Layout.flat [barre_depart; Layout.resident button_select_departure_on_map];
    route_depart_clip;
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident label_arrivee;
    Layout.flat [barre_arrivee; Layout.resident button_select_arrival_on_map];
    route_arrivee_clip;
    Layout.empty ~w:1 ~h:10 ();
    block_horaire;
    Layout.empty ~w:resultats_row_width ~h:resultats_row_height ();
    Layout.empty ~w:1 ~h:8 ();
    Layout.resident button_pmr_transfers;
    Layout.empty ~w:1 ~h:10 ();
    Layout.resident button_validate_search;
  ] in

  let tab_content_settings = Layout.tower ~align:Draw.Center [
    Layout.resident titre_panneau;
    spacer;
    block_actions;
    spacer;
    block_algo;
    spacer;
    weights_block;
    Layout.empty ~w:1 ~h:8 ();
    combination_block;
    spacer;
    block_cat;
  ] in

  Layout.set_show tab_content_route false;
  Layout.set_show tab_content_schedules false;
  Layout.set_show tab_content_results false;

  (* ================================================== *)
  (*                 BARRE D'ONGLETS                   *)
  (* ================================================== *)

  let zone_onglet = Layout.superpose [tab_content_settings; tab_content_route; tab_content_schedules; tab_content_results] in
  let h_zone_onglet = max tab_zone_min_height (!hauteur - tab_zone_height_margin) in
  let tabs_content_scroll =
    Layout.make_clip ~scrollbar:true ~scrollbar_width ~h:h_zone_onglet zone_onglet
  in

  let tab_settings = Widget.button "[ REGLAGE ]" in
  let tab_route = Widget.button "[ ITINÉRAIRE ]" in
  let tab_schedules = Widget.button "[ HORAIRES ]" in
  let tab_results = Widget.button "[ RESULTATS ]" in

  let tabs_bar = Layout.flat ~align:Draw.Center [
    Layout.resident ~w:tab_button_width tab_settings;
    Layout.resident ~w:tab_button_width tab_route;
    Layout.resident ~w:tab_button_width tab_schedules;
    Layout.resident ~w:tab_button_width tab_results;
  ] in

  (* ================================================== *)
  (*              ASSEMBLAGE GLOBAL UI                 *)
  (* ================================================== *)

  let fond_gris = Layout.color_bg (rgb_to_bogue (236, 239, 243)) in
  let left_panel = Layout.tower ~background:fond_gris ~align:Draw.Center [
    tabs_bar;
    spacer;
    tabs_content_scroll;
  ] in
  Layout.set_size left_panel ~w:panel_width ~h:!hauteur;

  let layout_carte = Layout.resident ~w:!canvas_w ~h:!canvas_h widget_canvas in

  let interface = Layout.flat [
    left_panel;
    layout_carte;
  ] in
  Layout.set_size interface ~w:!largeur ~h:!hauteur;

  (* ================================================== *)
  (*                RECORD DE SORTIE                   *)
  (* ================================================== *)

  {
    widget_canvas;
    button_station_names;
    category_buttons;
    algorithm_buttons;
    weight_buttons;
    current_hour;
    current_minute;
    is_departure_mode;
    button_departure_arrival;
    button_now;
    button_hour_minus;
    label_hour;
    button_hour_plus;
    button_minute_minus;
    label_minute;
    button_minute_plus;
    update_time_display;
    slider_time;
    slider_distance;
    slider_stops;
    slider_transfers;
    weights_block;
    sliders_box;
    combination_block;
    input_depart;
    input_arrivee;
    route_depart_keys;
    route_depart_buttons;
    route_depart_rows;
    route_arrivee_keys;
    route_arrivee_buttons;
    route_arrivee_rows;
    button_select_departure_on_map;
    button_select_arrival_on_map;
    button_pmr_transfers;
    button_validate_search;
    input_schedule_line;
    input_schedule_stop;
    label_schedule_info;
    label_schedule_table;
    input_result_details;
    result_display;
    result_keys;
    result_buttons;
    color_cells;
    result_rows;
    direction_buttons;
    direction_rows;
    table_labels;
    table_rows;
    tab_content_settings;
    tab_content_route;
    tab_content_schedules;
    tab_content_results;
    tab_settings;
    tab_route;
    tab_schedules;
    tab_results;
    tabs_bar;
    tabs_content_scroll;
    spacer;
    left_panel;
    layout_carte;
    interface;
  }
