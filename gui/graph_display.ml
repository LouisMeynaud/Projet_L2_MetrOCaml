(* ------------------TYPE ET CONSTANTE------------------- *)

type noeud = { id : string;nom : string;x : float;y :float}


type arete = { source : string;destination : string;couleur : int;categorie : string;groupe : string}

type donnees_affichage = {noeuds : noeud list;aretes : arete list;positions : (string * (int * int)) list;categories : (string * bool) list;bornes : float * float * float * float;}
let largeur = 1200
let hauteur = 800
let marge = 50
let btn_w = 80
let btn_h = 30
let zoom_in_factor = 1.15
let zoom_out_factor = 1.0 /. 1.15

(*---------------FONCTION UTILITAIRE---------------*)

(* recherche une valeur dans une liste associative
  @param cle la clé à rechercher
  @param liste la liste associative
  @param defaut la valeur par défaut si la clé n'est pas trouvée *)
let cherche cle liste defaut =
  match List.assoc_opt cle liste with
  | Some valeur -> valeur
  | None -> defaut

(* Calcule les borne min/max de l'ensemble des noeuds 
  @param noeuds la liste des noeuds 
  return un tuple (min_x,max_x,min_y,max_y)*)
let calcule_bornes noeuds =
  let xs = List.map (fun n -> n.x) noeuds in
  let ys = List.map (fun n -> n.y) noeuds in
  (List.fold_left min max_float xs, 
   List.fold_left max min_float xs,
   List.fold_left min max_float ys, 
   List.fold_left max min_float ys)

(* convertit une coordonnée reel en coordonée écran
(applique une mise a l'echele pour que tout rentre dans l'ecran)
@param bornes les bornes de l'ensemble des noeuds
@param x la coordonnée x réelle
@param y la coordonnée y réelle *)
let to_screen bornes x y =
  let (min_x, max_x, min_y, max_y) = bornes in
  let echelle = min 
    (float (largeur - 2*marge) /. (max_x -. min_x)) 
    (float (hauteur - 2*marge) /. (max_y -. min_y)) in
  (marge + int_of_float ((x -. min_x) *. echelle),
   marge + int_of_float ((y -. min_y) *. echelle))

(*calcule les position initiales pour tout les noeud
  return une linste associative (id,(x_pixel,y_pixel));..
  @param bornes les bornes de l'ensemble des noeuds
  @param noeuds la liste des noeuds*)
let calcule_positions bornes noeuds =
  List.map (fun n -> (n.id, to_screen bornes n.x n.y)) noeuds

(* applique un deplacement a toute les positions
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param dx le deplacement en
  @param dy le deplacement en
  return une liste associative (id,(x_pixel,y_pixel)) *)
let move positions dx dy =
  List.map (fun (id, (x, y)) -> (id, (x + dx, y + dy))) positions

(* applique un zoom
   @param positions la liste associative (id,(x_pixel,y_pixel))
   @param cx centre x du zoom (position souris)
   @param cy centre y du zoom
   @param facteur facteur de zoom (>1 = zoom in, <1 = zoom out)
   return nouvelle liste de positions *)
let zoom_positions positions cx cy facteur =
  List.map (fun (id, (x, y)) ->
    let nx = cx + int_of_float (float (x - cx) *. facteur) in
    let ny = cy + int_of_float (float (y - cy) *. facteur) in
    (id, (nx, ny))
  ) positions

(*---------------FONCTION DE DESSIN----------------*)

(* dessine une arrete (ligne) entre deux position donée
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param arete l'arrete à dessiner *)
let draw_arete positions arete =
  let (x1, y1) = cherche arete.source positions (0, 0) in
  let (x2, y2) = cherche arete.destination positions (0, 0) in
  Graphics.set_color arete.couleur;
  Graphics.set_line_width 3;
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2

(* dessine une noeud et son nom à la position donnée
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param noeud le noeud à dessiner *)
let draw_noeud positions noeud =
  let (x, y) = cherche noeud.id positions (0, 0) in
  Graphics.set_color Graphics.black;
  Graphics.fill_circle x y 10;
  Graphics.set_color Graphics.white;
  Graphics.fill_circle x y 7;
  Graphics.set_color Graphics.black;
  Graphics.moveto (x + 12) (y - 4);
  Graphics.draw_string noeud.nom

(* dessine un bouton pour une categorie(vert si actif gris sinon)
  @param i l'index du bouton
  @param (nom, actif) le nom et l'état du bouton *)
let draw_bouton i (nom, actif) =
  let x = 10 + i * (btn_w + 10) in
  let y = hauteur - btn_h - 10 in
  Graphics.set_color (if actif then 0x4CAF50 else 0xCCCCCC);
  Graphics.fill_rect x y btn_w btn_h;
  Graphics.set_color Graphics.black;
  Graphics.draw_rect x y btn_w btn_h;
  Graphics.moveto (x + 10) (y + 8);
  Graphics.draw_string nom

(* fonction de rafraichissement de l'ecran
  1. efface l'ecran
  2. filtre et dessine les aretes visibles
  3. filtre et dessine les noeuds par dessus
  4. dessine les boutons 
  
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param noeuds la liste des noeuds
  @param aretes la liste des aretes
  @param categories la liste des categories (pour filtrer)*)
let redraw positions noeuds aretes categories =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 largeur hauteur;

  let arrete_visibles = List.filter (fun a -> cherche a.categorie categories true) aretes in
  let noeuds_visibles = List.filter (fun n ->
    List.exists (fun a -> a.source = n.id || a.destination = n.id) arrete_visibles
  ) noeuds in
  List.iter (draw_arete positions) arrete_visibles;
  List.iter (draw_noeud positions) noeuds_visibles;
  List.iteri draw_bouton categories;
  Graphics.synchronize ()

(*---------------FOCNTION INTERACTION----------------*)

(*verifie si un clic (x ,y) est sur un bouton
  @param x la coordonnée x du clic
  @param y la coordonnée y du clic
  @param n le nombre de boutons
  return Some i si le clic est sur le bouton i, None sinon *)
let clic_bouton x y n =
  let by = hauteur - btn_h - 10 in
  if y >= by && y <= by + btn_h then
    let i = (x - 10) / (btn_w + 10) in
    if i >= 0 && i < n then Some i else None
  else None

(* inverse l'etat actif/inactif d'une categorie donnée par sont index i*)
let toggle categories i =
  List.mapi (fun j (c, v) -> (c, if i = j then not v else v)) categories

(* boucle pour gerer le mouvement de la camera
  fonction recusive (tourne tant que l'on ne relace pas le bouton de la sourie)
  met a jour les positions des noeuds 
  
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param noeuds la liste des noeuds
  @param aretes la liste des aretes
  @param categories la liste des categories (pour filtrer)
  @param ancien_x l'ancienne coordonnée x de la sourie
  @param ancien_y l'ancienne coordonnée y de la sourie *)
let rec drag positions noeuds aretes categories ancien_x ancien_y =
  if not (Graphics.button_down ()) then
    positions
  else
    let (mx, my) = Graphics.mouse_pos () in
    let dx = mx - ancien_x in
    let dy = my - ancien_y in
    
    if dx = 0 && dy = 0 then
      drag positions noeuds aretes categories ancien_x ancien_y
    else
      let nouvelles_pos = move positions dx dy in
      
      redraw nouvelles_pos noeuds aretes categories;
      drag nouvelles_pos noeuds aretes categories mx my

(* boucle principale 
  attend un clique ou une touche et decide de quoi faire
  - clic sur bouton -> toggle categorie
  - clic ailleurs -> drag camera
  - touche + -> zoom in
  - touche - -> zoom out *)
let rec boucle positions noeuds aretes categories =
  let ev = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
  
  (* Gestion du zoom clavier *)
  if ev.Graphics.keypressed then begin
    let (mx, my) = Graphics.mouse_pos () in
    let key = ev.Graphics.key in
    let zoom = if key = '+' ||key = '=' then zoom_in_factor else zoom_out_factor in
      let positions' = zoom_positions positions mx my zoom in
      redraw positions' noeuds aretes categories;
      boucle positions' noeuds aretes categories
  end
  else begin
    (* Gestion des clics *)
    match clic_bouton ev.Graphics.mouse_x ev.Graphics.mouse_y (List.length categories) with
    | Some i -> 
        (* Cas 1 : Clic sur un bouton -> on toggle la categorie et on redraw *)
        let categories = toggle categories i in
        redraw positions noeuds aretes categories; 
        boucle positions noeuds aretes categories
    | None -> 
        (* Cas 2 : Clic dans le vide -> On lance la fonction drag *)
        let positions_bis = drag positions noeuds aretes categories ev.Graphics.mouse_x ev.Graphics.mouse_y in
        boucle positions_bis noeuds aretes categories
  end
(*----------------FONCTIONS PRINCIPALE------------------*)

(*main*)
let run noeuds aretes =
  let bornes = calcule_bornes noeuds in
  let positions = calcule_positions bornes noeuds in
  let cats = List.map (fun a -> a.categorie) aretes in
  let categories = List.map (fun c -> (c, true)) (List.sort_uniq String.compare cats) in
  Graphics.open_graph (" " ^ string_of_int largeur ^ "x" ^ string_of_int hauteur);
  Graphics.auto_synchronize false;
  redraw positions noeuds aretes categories;
  boucle positions noeuds aretes categories
