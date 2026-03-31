(* gui_types.ml *)

(* ------------------ Types partages GUI ------------------- *)

type noeud = { id : string;nom : string;x : float;y :float}
type arete = { source : string;destination : string;couleur : int;categorie : string;groupe : string}

type etat = {
  noeuds : noeud list;
  aretes : arete list;
  positions : (string, (float * float)) Hashtbl.t;
  noeuds_visibles : noeud list;
  aretes_groupees : ((string * string), arete list) Hashtbl.t;
  noeuds_actifs : (string, bool) Hashtbl.t;
  categories : (string * bool) list;
  source : string option;
  dest : string option;
  show_names : bool;
  algos : string list;
  algo_actif : string;
  poids_list : string list;
  poids_actif : string;
  camera_x : float;
  camera_y : float;
  zoom : float;
}

type schedule_item = {
  mode_name : string;
  line_name : string;
  line_color : int;
  station_id : string;
  station_name : string;
  direction : string;
  times : int list;
}

(* ------------------ Parametres UI globaux ------------------- *)

let largeur = ref 1200 (* Largeur de la fenetre principale. *)
let hauteur = ref 800 (* Hauteur de la fenetre principale. *)
let panel_w = 250 (* Largeur de reference du panneau gauche. *)
let canvas_w = ref (!largeur - panel_w) (* Largeur de la zone de carte. *)
let canvas_h = ref !hauteur (* Hauteur de la zone de carte. *)

let window_min_width = 720 (* Largeur minimale pour la fenetre. *)
let window_min_height = 520 (* Hauteur minimale pour la fenetre. *)
let window_screen_padding = 16 (* Marge retiree des dimensions ecran detectees. *)

let panel_ratio_percent = 30 (* Ratio du panneau sur la largeur totale. *)
let panel_min_width = 210 (* Largeur minimale du panneau gauche. *)
let panel_max_width = 360 (* Largeur maximale du panneau gauche. *)
let canvas_min_width = 320 (* Largeur minimale preservee pour la carte. *)

let marge = 50 (* Marge interne appliquee au rendu de la carte. *)
let btn_w = 80 (* Largeur de base des boutons standards. *)
let btn_h = 30 (* Hauteur de base des boutons standards. *)

let zoom_in_factor = 1.15 (* Facteur de zoom avant par cran molette. *)
let zoom_out_factor = 1.0 /. 1.15 (* Facteur de zoom arriere par cran molette. *)
let min_zoom = 0.5 (* Niveau de zoom minimal autorise. *)
let max_zoom = 90.0 (* Niveau de zoom maximal autorise. *)

let draw_edge_thickness = 3 (* Epaisseur des traits d'aretes. *)
let draw_edge_parallel_offset = 4.0 (* Decalage entre aretes paralleles. *)
let draw_edge_clip_padding = 32.0 (* Marge de clipping pour le dessin des aretes. *)
let draw_node_clip_padding = 20.0 (* Marge de clipping pour le dessin des noeuds. *)
let draw_node_radius_outer = 5 (* Rayon externe d'un noeud standard. *)
let draw_node_radius_inner = 4 (* Rayon interne d'un noeud standard. *)
let draw_node_selected_radius_outer = 9 (* Rayon externe d'un noeud selectionne. *)
let draw_node_selected_radius_inner = 8 (* Rayon interne d'un noeud selectionne. *)
let draw_click_radius_squared = 400.0 (* Distance max de selection au carre. *)

let panel_slider_min_length = 80 (* Longueur minimale d'un slider dans le panneau. *)
let panel_slider_width_margin = 140 (* Espace reserve dans la ligne slider (labels + marge). *)
let search_input_min_width = 110 (* Largeur minimale des champs de recherche. *)
let search_input_width_margin = 120 (* Espace reserve a cote des champs de recherche. *)
let route_max_suggestions = 20 (* Nombre max de suggestions affichees pour depart/arrivee. *)
let route_suggestion_row_height = 28 (* Hauteur d'une ligne de suggestion d'itineraire. *)
let route_suggestion_clip_height = 120 (* Hauteur du panneau scrollable des suggestions d'itineraire. *)

let tab_zone_min_height = 220 (* Hauteur minimale de la zone d'onglets scrollable. *)
let tab_zone_height_margin = 70 (* Marge verticale retiree pour dimensionner la zone d'onglets. *)
let tab_button_width = 110 (* Largeur fixe de chaque bouton d'onglet. *)
let scrollbar_width = 8 (* Largeur des barres de defilement dans les clips. *)

let horaires_row_min_width = 150 (* Largeur minimale d'une ligne de resultats horaires. *)
let horaires_row_width_margin = 26 (* Marge retiree pour calculer la largeur des lignes horaires. *)
let horaires_row_height = 30 (* Hauteur d'une ligne de resultats horaires. *)
let horaires_color_cell_width = 16 (* Largeur de la cellule de couleur par resultat. *)
let horaires_max_search_results = 60 (* Nombre max de resultats horaires affiches. *)
let horaires_max_direction_buttons = 6 (* Nombre max de boutons de direction affiches. *)
let horaires_max_table_rows = 24 (* Nombre max de lignes dans la table des passages. *)
let horaires_result_clip_height = 220 (* Hauteur du panneau scrollable des resultats. *)
let horaires_direction_clip_height = 110 (* Hauteur du panneau scrollable des directions. *)
let horaires_table_clip_height = 250 (* Hauteur du panneau scrollable des horaires detailles. *)
let horaires_table_row_height = 26 (* Hauteur d'une ligne dans la table de passages. *)
let max_resize_safety_threshold = 50 (* Taille minimale valide lors d'un resize. *)
