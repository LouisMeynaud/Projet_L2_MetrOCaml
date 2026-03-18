(* graph_display.ml *)
open Bogue
open Gui_types
open Gui_draw


(* ================================================== *)
(*                       RUN                          *)
(* ================================================== *)
let run noeuds aretes algos poids_list on_search =

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
  largeur := max 720 (screen_w - 16);
  hauteur := max 520 (screen_h - 16);

  (* Le panneau prend ~30% de la largeur, avec bornes pour petits/grands ecrans. *)
  let panel_width =
    let preferred = !largeur * 30 / 100 in
    max 210 (min 360 preferred)
  in

  canvas_w := max 320 (!largeur - panel_width);
  canvas_h := !hauteur;

  let bornes = calcule_bornes noeuds in
  let cats = List.sort_uniq String.compare (List.map (fun (a : arete) -> a.categorie) aretes) in

  let widget_dessin = Widget.sdl_area ~w:!canvas_w ~h:!canvas_h () in
  mon_canvas := Some (Widget.get_sdl_area widget_dessin);

  let etat_initial = {
    noeuds; aretes;
    positions = calcule_positions bornes noeuds;
    categories = List.map (fun c -> (c, true)) cats;
    source = None; dest = None; show_names = false;
    algos; algo_actif = List.hd algos;
    poids_list; poids_actif = List.hd poids_list;
  } in
  

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
  let slider_len = max 80 (panel_width - 140) in
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
  let search_input_w = max 110 (panel_width - 120) in
  let barre_depart = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_depart in
  let barre_arrivee = Layout.resident ~w:search_input_w ~background:fond_barre_recherche input_arrivee in

  let station_name_of_id id =
    match List.find_opt (fun (n : noeud) -> n.id = id) noeuds with
    | Some n -> n.nom
    | None -> id
  in

  let station_id_of_name nom =
    let cible = String.lowercase_ascii (String.trim nom) in
    if cible = "" then None
    else
      List.find_opt
        (fun (n : noeud) -> String.lowercase_ascii n.nom = cible)
        noeuds
      |> Option.map (fun n -> n.id)
  in

  let mode_selection_carte = ref `Aucun in
  
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

  (* Les contenus d'onglets se superposent pour conserver la meme position visuelle. *)
  let zone_onglet = Layout.superpose [contenu_itineraire; contenu_recherche] in
  let h_zone_onglet = max 220 (!hauteur - 70) in
  let zone_onglet_scroll = Layout.make_clip ~scrollbar:true ~scrollbar_width:8 ~h:h_zone_onglet zone_onglet in

  (* -------------------- Barre d'onglets -------------------- *)

  let tab_itineraire = Widget.button "[ REGLAGE ]" in
  let tab_recherche = Widget.button "[ ITINÉRAIRE ]" in
  let tab_horaires = Widget.button "[ HORAIRES ]" in
  
  let barre_onglets = Layout.flat ~align:Draw.Center [
    Layout.resident tab_itineraire;
    Layout.resident tab_recherche;
    Layout.resident tab_horaires;
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

  let switch_tab ~show_itineraire ~show_recherche =
    Layout.set_show contenu_itineraire show_itineraire;
    Layout.set_show contenu_recherche show_recherche;
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
    (* preparation de la recuperation du bool depart et de l'heure de depart , suprimer les ignore et les passer a on_search une fois les algo de temps fini*)
    let depart = !is_depart in
    let time_in_secondes = (!curr_heure * 3600) + (!curr_minute * 60) in
    ignore(time_in_secondes); (* pour l'instant on ne l'utilise pas, mais ça permet d'éviter un warning *)
    ignore depart; (* même chose pour le mode départ/arrivée *)
    let c_temps = float_of_int (Slider.value (Widget.get_slider s_temps)) /. 100.0 in
    let c_dist = float_of_int (Slider.value (Widget.get_slider s_dist)) /. 100.0 in
    let c_arrets = float_of_int (Slider.value (Widget.get_slider s_arrets)) /. 100.0 in
    let c_corresp = float_of_int (Slider.value (Widget.get_slider s_corresp)) /. 100.0 in
    on_search source dest !current_etat.algo_actif !current_etat.poids_actif [c_temps; c_dist; c_arrets; c_corresp]
  in

  (* Connexions des Onglets *)
  let conn_tab_iti = Widget.connect tab_itineraire tab_itineraire (fun _ _ _ ->
    switch_tab ~show_itineraire:true ~show_recherche:false
  ) Trigger.buttons_down in

  let conn_tab_rech = Widget.connect tab_recherche tab_recherche (fun _ _ _ ->
    switch_tab ~show_itineraire:false ~show_recherche:true
  ) Trigger.buttons_down in

  let conn_select_depart = Widget.connect btn_select_depart btn_select_depart (fun _ _ _ ->
    begin_map_selection `Depart
  ) Trigger.buttons_down in

  let conn_select_arrivee = Widget.connect btn_select_arrivee btn_select_arrivee (fun _ _ _ ->
    begin_map_selection `Arrivee
  ) Trigger.buttons_down in

  (* Action de recherche depuis l'onglet recherche *)
  let conn_valider_rech = Widget.connect btn_valider_recherche btn_valider_recherche (fun _ _ _ ->
    let id_dep = station_id_of_name (Widget.get_text input_depart) in
    let id_arr = station_id_of_name (Widget.get_text input_arrivee) in
    let source = match id_dep with Some id -> Some id | None -> !current_etat.source in
    let dest = match id_arr with Some id -> Some id | None -> !current_etat.dest in
    current_etat := { !current_etat with source; dest };
    update_ui_buttons ();
    rafraichir_ecran !current_etat;
    match source, dest with
    | Some s, Some d -> launch_search s d
    | _ -> print_endline "Entrez un depart et une arrivee valides, ou selectionnez-les sur la carte."
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
      current_etat := { !current_etat with categories = new_cats };
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

  (* ================================================== *)
  (*                 EVENEMENTS CARTE                  *)
  (* ================================================== *)

  let is_dragging = ref false in
  let debut_x = ref 0 in
  let debut_y = ref 0 in
  let dx_glisse = ref 0 in
  let dy_glisse = ref 0 in
  let zoom_level = ref 1.0 in
  let min_zoom = 0.5 in
  let max_zoom = 7.0 in
  let pos_initiales = ref [] in
  let needs_redraw = ref false in

  let boucle _w _target ev =
    let etat = !current_etat in

    match Trigger.event_kind ev with

    | `Mouse_wheel ->
      let wheel_delta = Tsdl.Sdl.Event.(get ev mouse_wheel_y) in
      if wheel_delta <> 0 then begin
        let zoom = if wheel_delta > 0 then zoom_in_factor else zoom_out_factor in
        let next_zoom = !zoom_level *. zoom in
        if next_zoom >= min_zoom && next_zoom <= max_zoom then begin
          zoom_level := next_zoom;
          let mx, my = !canvas_w / 2, !canvas_h / 2 in
          let etat' = { etat with positions = zoom_positions etat.positions mx my zoom } in
          current_etat := etat';
          needs_redraw := true
        end
      end

    | `Mouse_button_down | `Finger_down ->
      let mx, my = Sdl_area.pointer_pos (get_canvas ()) ev in
      let etat' =
        match !mode_selection_carte, get_noeud_clique mx my etat with
        | `Depart, Some id ->
          Widget.set_text input_depart (station_name_of_id id);
          Widget.set_text btn_select_depart "Selection carte";
          mode_selection_carte := `Aucun;
          { etat with source = Some id }
        | `Arrivee, Some id ->
          Widget.set_text input_arrivee (station_name_of_id id);
          Widget.set_text btn_select_arrivee "Selection carte";
          mode_selection_carte := `Aucun;
          { etat with dest = Some id }
        | (`Depart | `Arrivee), None -> etat
        | `Aucun, _ -> etat
      in
      
      if etat' = etat then begin
        is_dragging := true;
        debut_x := mx;
        debut_y := my;
        dx_glisse := 0;
        dy_glisse := 0;
        pos_initiales := etat.positions
      end else begin
        current_etat := etat';
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
        current_etat := { etat with positions = move !pos_initiales !dx_glisse !dy_glisse };
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
    List.map (fun (id, (x, y)) -> (id, (x *. sx, y *. sy))) positions
  in
  (* Fonction pour réinitialiser l'état de glissement après un zoom ou un resize *)
  let reset_drag_state () =
    is_dragging := false;
    dx_glisse := 0;
    dy_glisse := 0;
    pos_initiales := !current_etat.positions
  in

  (* Lors d'un resize de la fenêtre, on doit redimensionner les positions des noeuds pour qu'ils restent à la bonne place sur la carte *)
  let apply_map_resize () =
    let old_w = max 1 !canvas_w in
    let old_h = max 1 !canvas_h in
    let new_w, new_h = Layout.get_physical_size layout_carte in
    let size_changed = new_w > 50 && new_h > 50 && (new_w <> old_w || new_h <> old_h) in
    if size_changed then begin
      canvas_w := new_w;
      canvas_h := new_h;
      current_etat := {
        !current_etat with
        positions = scale_positions ~from_w:old_w ~from_h:old_h ~to_w:new_w ~to_h:new_h !current_etat.positions
      };
      reset_drag_state ();
      needs_redraw := true
    end
  in

  Layout.on_resize layout_carte apply_map_resize;

  (* On rassemble toutes les connexions pour les passer à la fenêtre *)
  let connexions = connexion_carte :: conn_tab_iti :: conn_tab_rech :: conn_select_depart :: conn_select_arrivee :: conn_valider_rech :: conn_noms :: conn_dep :: conn_maint :: conn_hm :: conn_hp :: conn_mm :: conn_mp :: (conn_cats @ conn_algos @ conn_poids) in
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

  Main.run ~before_display:rafraichissement fenetre