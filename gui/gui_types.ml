(* gui_types.ml *)

(* ------------------TYPE ET CONSTANTE------------------- *)

type noeud = { id : string;nom : string;x : float;y :float}
type arete = { source : string;destination : string;couleur : int;categorie : string;groupe : string}

type etat = {
  noeuds : noeud list;
  aretes : arete list;
  positions : (string * (float * float)) list;
  categories : (string * bool) list;
  source : string option;
  dest : string option;
  show_names : bool;
  algos : string list;
  algo_actif : string;
  poids_list : string list;
  poids_actif : string;
}
let largeur = ref 1200
let hauteur = ref 800
let panel_w = 250 (* Largeur du panneau gauche *)
let canvas_w = ref (!largeur - panel_w) (* La carte prend la place restante *)
let canvas_h = ref !hauteur
let marge = 50
let btn_w = 80
let btn_h = 30
let zoom_in_factor = 1.15
let zoom_out_factor = 1.0 /. 1.15