open Bogue
open Gui_types
open Gui_draw
open Gui_view

let run noeuds aretes algos poids_list schedule_items on_search =
  ignore (Tsdl.Sdl.init Tsdl.Sdl.Init.video);

  let screen_w, screen_h =
    match Tsdl.Sdl.get_display_usable_bounds 0 with
    | Ok rect -> (Tsdl.Sdl.Rect.w rect, Tsdl.Sdl.Rect.h rect)
    | Error _ ->
      (match Tsdl.Sdl.get_desktop_display_mode 0 with
       | Ok mode -> (mode.Tsdl.Sdl.dm_w, mode.Tsdl.Sdl.dm_h)
       | Error _ -> (!largeur, !hauteur))
  in

  largeur := max window_min_width (screen_w - window_screen_padding);
  hauteur := max window_min_height (screen_h - window_screen_padding);

  let panel_width =
    let preferred = !largeur * panel_ratio_percent / 100 in
    max panel_min_width (min panel_max_width preferred)
  in

  canvas_w := max canvas_min_width (!largeur - panel_width);
  canvas_h := !hauteur;

  let bornes = calcule_bornes noeuds in
  let cats = List.sort_uniq String.compare (List.map (fun (a : arete) -> a.categorie) aretes) in

  let etat_initial = {
    noeuds; 
    aretes;
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
  let view = Gui_view.build ~panel_width ~etat_initial in

  let connections, apply_map_resize, before_display =
    Gui_controller.setup ~noeuds ~schedule_items ~on_search ~current_etat ~view ~layout_carte:view.layout_carte
  in

  Layout.on_resize view.layout_carte apply_map_resize;

  let fenetre = Main.of_layout ~connections view.interface in
  apply_map_resize ();
  rafraichir_ecran !current_etat;

  Main.run ~before_display fenetre;
  clear_label_cache ()
