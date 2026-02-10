

type noeud = { 
  id : string;
  nom : string;
  x : float;  
  y : float
}


type arete = { 
  source : string;       
  destination : string; 
  couleur : int;
  categorie : string;
  groupe : string
}

type donnees_affichage = {
  noeuds : noeud list;
  aretes : arete list;
  positions : (string * (int * int)) list;
  categories : (string * bool) list;
  bornes : float * float * float * float;
}
let largeur = 1200
let hauteur = 800
let marge = 50

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
