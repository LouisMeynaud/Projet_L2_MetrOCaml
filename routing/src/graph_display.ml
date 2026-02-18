

type noeud = { id : string;nom : string;x : float;y :float}


type arete = { source : string;destination : string;couleur : int;categorie : string;groupe : string}

type donnees_affichage = {noeuds : noeud list;aretes : arete list;positions : (string * (int * int)) list;categories : (string * bool) list;bornes : float * float * float * float;}
let largeur = 1200
let hauteur = 800
let marge = 50
let btn_w = 80
let btn_h = 30

let cherche cle liste defaut =
  match List.assoc_opt cle liste with
  | Some valeur -> valeur
  | None -> defaut

let calcule_bornes noeuds =
  let xs = List.map (fun n -> n.x) noeuds in
  let ys = List.map (fun n -> n.y) noeuds in
  (List.fold_left min max_float xs, 
   List.fold_left max min_float xs,
   List.fold_left min max_float ys, 
   List.fold_left max min_float ys)

let to_screen bornes x y =
  let (min_x, max_x, min_y, max_y) = bornes in
  let echelle = min 
    (float (largeur - 2*marge) /. (max_x -. min_x)) 
    (float (hauteur - 2*marge) /. (max_y -. min_y)) in
  (marge + int_of_float ((x -. min_x) *. echelle),
   marge + int_of_float ((y -. min_y) *. echelle))

let calcule_positions bornes noeuds =
  List.map (fun n -> (n.id, to_screen bornes n.x n.y)) noeuds



let draw_arete positions arete =
  let (x1, y1) = cherche arete.source positions (0, 0) in
  let (x2, y2) = cherche arete.destination positions (0, 0) in
  Graphics.set_color arete.couleur;
  Graphics.set_line_width 3;
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2


let draw_noeud positions noeud =
  let (x, y) = cherche noeud.id positions (0, 0) in
  Graphics.set_color Graphics.black;
  Graphics.fill_circle x y 10;
  Graphics.set_color Graphics.white;
  Graphics.fill_circle x y 7;
  Graphics.set_color Graphics.black;
  Graphics.moveto (x + 12) (y - 4);
  Graphics.draw_string noeud.nom

let draw_bouton i (nom, actif) =
  let x = 10 + i * (btn_w + 10) in
  let y = hauteur - btn_h - 10 in
  Graphics.set_color (if actif then 0x4CAF50 else 0xCCCCCC);
  Graphics.fill_rect x y btn_w btn_h;
  Graphics.set_color Graphics.black;
  Graphics.draw_rect x y btn_w btn_h;
  Graphics.moveto (x + 10) (y + 8);
  Graphics.draw_string nom

let redraw positions noeuds aretes categories =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 largeur hauteur;
  let visibles = List.filter (fun a -> cherche a.categorie categories true) aretes in
  List.iter (draw_arete positions) visibles;
  List.iter (draw_noeud positions) noeuds;
  List.iteri draw_bouton categories;
  Graphics.synchronize ()

let clic_bouton x y n =
  let by = hauteur - btn_h - 10 in
  if y >= by && y <= by + btn_h then
    let i = (x - 10) / (btn_w + 10) in
    if i >= 0 && i < n then Some i else None
  else None

let toggle categories i =
  List.mapi (fun j (c, v) -> (c, if i = j then not v else v)) categories

let rec boucle positions noeuds aretes categories =
  let ev = Graphics.wait_next_event [Graphics.Button_down] in
  match clic_bouton ev.Graphics.mouse_x ev.Graphics.mouse_y (List.length categories) with
  | Some i -> 
      let categories = toggle categories i in
      redraw positions noeuds aretes categories; 
      boucle positions noeuds aretes categories
  | None -> 
      boucle positions noeuds aretes categories
let run noeuds aretes =
  let bornes = calcule_bornes noeuds in
  let positions = calcule_positions bornes noeuds in
  let cats = List.map (fun a -> a.categorie) aretes in
  let categories = List.map (fun c -> (c, true)) (List.sort_uniq String.compare cats) in
  Graphics.open_graph (" " ^ string_of_int largeur ^ "x" ^ string_of_int hauteur);
  Graphics.auto_synchronize false;
  redraw positions noeuds aretes categories;
  boucle positions noeuds aretes categories
