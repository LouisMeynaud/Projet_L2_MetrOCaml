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
  (float marge +. (x -. min_x) *. echelle,
   float marge +. (y -. min_y) *. echelle)

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
  List.map (fun (id, (x, y)) -> (id, (x +. float dx, y +. float dy))) positions

(* applique un zoom
   @param positions la liste associative (id,(x_pixel,y_pixel))
   @param cx centre x du zoom (position souris)
   @param cy centre y du zoom
   @param facteur facteur de zoom (>1 = zoom in, <1 = zoom out)
   return nouvelle liste de positions *)
let zoom_positions positions cx cy facteur =
  List.map (fun (id, (x, y)) ->
    let nx = (float cx) +. (x -. (float cx)) *. facteur in
    let ny = (float cy) +. (y -. (float cy)) *. facteur in
    (id, (nx, ny))
  ) positions


(*corrige les strings afin de retirer les accents non reconnue par Graphics*)
let clean_accents s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then Buffer.contents b
    else match s.[i] with
      | '\xC3' when i + 1 < String.length s ->
          let next = s.[i+1] in
          let c = match next with
            | '\xA0' | '\xA2' | '\xA4' -> 'a' (* à, â, ä *)
            | '\x80' | '\x82' | '\x84' -> 'A' (* À, Â, Ä *)
            | '\xA7' -> 'c' (* ç *)
            | '\x87' -> 'C' (* Ç *)
            | '\xA8' | '\xA9' | '\xAA' | '\xAB' -> 'e' (* è, é, ê, ë *)
            | '\x88' | '\x89' | '\x8A' | '\x8B' -> 'E' (* È, É, Ê, Ë *)
            | '\xAE' | '\xAF' -> 'i' (* î, ï *)
            | '\x8E' | '\x8F' -> 'I' (* Î, Ï *)
            | '\xB4' | '\xB6' -> 'o' (* ô, ö *)
            | '\x94' | '\x96' -> 'O' (* Ô, Ö *)
            | '\xB9' | '\xBB' | '\xBC' -> 'u' (* ù, û, ü *)
            | '\x99' | '\x9B' | '\x9C' -> 'U' (* Ù, Û, Ü *)
            | _ -> '?'
          in
          Buffer.add_char b c;
          loop (i + 2)
      | c -> 
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0


let distance_carre px py mx my =
  let dx = px -. float mx in
  let dy = py -. float my in
  dx *. dx +. dy *. dy

let get_noeud_clique mx my etat =
  let arrete_visibles = List.filter (fun (a : arete) -> cherche a.categorie etat.categories true) etat.aretes in
  let noeuds_actifs = Hashtbl.create 5000 in
  List.iter (fun (a : arete) -> 
  Hashtbl.replace noeuds_actifs a.source true; 
  Hashtbl.replace noeuds_actifs a.destination true
) arrete_visibles;
  
  let (best_id, _) = List.fold_left (fun (best, min_d) (id, (px, py)) ->
      let d = distance_carre px py mx my in
      if d <= 400.0 && d < min_d && Hashtbl.mem noeuds_actifs id then (Some id, d) else (best, min_d)
    ) (None, max_float) etat.positions
  in best_id

(*---------------FONCTION DE DESSIN----------------*)
(* Cache du graphe rendu sous forme d'image.
   Permet d'éviter de redessiner tous les primitifs lors du déplacement. *)
let graph_cache : Graphics.image option ref = ref None

(* dessine une arrete (ligne) entre deux position donée
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param arete l'arrete à dessiner 
  @param i l'index de la ligne parmi les lignes parallèles
  @param n le nombre total de lignes parallèles *)
let draw_arete positions (arete : arete) i n =
  let (x1, y1) = cherche arete.source positions (0., 0.) in
  let (x2, y2) = cherche arete.destination positions (0., 0.) in
  let (ox, oy) =
    if n <= 1 then (0., 0.) (* une station : on decalle pas*)
    else 
      let (dx, dy) = if arete.source < arete.destination then (x2 -. x1, y2 -. y1) else (x1 -. x2, y1 -. y2) in
      let len = sqrt (dx *. dx +. dy *. dy) in
      if len = 0. then (0., 0.) 
      else
        let nx, ny = -.dy /. len, dx /. len in
        let shift = (float i -. (float (n - 1) /. 2.0)) *. 4.0 in (* decalle pour centré le paquet*)
        (nx *. shift, ny *. shift)
  in
  Graphics.set_color arete.couleur; 
  Graphics.set_line_width 3;
  Graphics.moveto (int_of_float (x1 +. ox)) (int_of_float (y1 +. oy));
  Graphics.lineto (int_of_float (x2 +. ox)) (int_of_float (y2 +. oy))

(*dessine un noeud*)
  let draw_noeud etat noeud =
  let (x, y) = cherche noeud.id etat.positions (0., 0.) in
  let (r_ext, r_int) =
    if Some noeud.id = etat.source then (Graphics.set_color Graphics.green; 9, 8)
    else if Some noeud.id = etat.dest then (Graphics.set_color Graphics.red; 9, 8)
    else (Graphics.set_color Graphics.black; 5, 4)
  in
  Graphics.fill_circle (int_of_float x) (int_of_float y) r_ext;
  Graphics.set_color Graphics.white; Graphics.fill_circle (int_of_float x) (int_of_float y) r_int;
  if etat.show_names then begin
    Graphics.set_color Graphics.black; Graphics.moveto (int_of_float x + 12) (int_of_float y - 4);
    Graphics.draw_string (clean_accents noeud.nom)
  end

(* dessine un bouton *)
let draw_bouton x y w h text color bg_color =
  Graphics.set_color bg_color; Graphics.fill_rect x y w h;
  Graphics.set_color color; Graphics.draw_rect x y w h;
  Graphics.moveto (x + 10) (y + 8); Graphics.draw_string (clean_accents text)
let draw_selection_row y_pos label boutons actif =
  Graphics.set_color Graphics.black; Graphics.moveto 5 (y_pos + 8); Graphics.draw_string label;
  List.iteri (fun i nom -> draw_bouton(60 + i * (btn_w + 5)) y_pos btn_w btn_h nom Graphics.black (if nom = actif then 0x2196F3 else 0xCCCCCC)) boutons

let draw_ui etat =
  List.iteri (fun i (nom, actif) -> draw_bouton (10 + i * (btn_w + 10)) (hauteur - btn_h - 10) btn_w btn_h nom Graphics.black (if actif then 0x4CAF50 else 0xCCCCCC)) etat.categories;
  draw_bouton (largeur - 120) (hauteur - 45) 100 35 "CHERCHER" Graphics.white Graphics.blue;
  draw_bouton (largeur - 240) (hauteur - 45) 110 35 (if etat.show_names then "NOMS : ON" else "NOMS : OFF") Graphics.black (if etat.show_names then 0x4CAF50 else 0xCCCCCC);
  draw_selection_row 10 "Algo :" etat.algos etat.algo_actif;
  if etat.algo_actif = "Dijkstra" then 
    draw_selection_row 50 "Poids:" etat.poids_list etat.poids_actif

(* dessine uniquement le graphe (aretes + noeuds), sans effacer ni synchroniser.
*)
let draw_graph etat =
  let categories = etat.categories in
  let aretes = etat.aretes in
  let noeuds = etat.noeuds in

  (*Filtrage des arêtes selon les catégories actives *)
  let arrete_visibles = List.filter (fun (a : arete) -> cherche a.categorie categories true) aretes in
  
  (*Identification des noeuds qui ont au moins une arête visible *)
  let noeuds_actifs = Hashtbl.create 15000 in
  List.iter (fun (a : arete) -> 
    Hashtbl.replace noeuds_actifs a.source true;
    Hashtbl.replace noeuds_actifs a.destination true
  ) arrete_visibles;

  let noeuds_visibles = List.filter (fun n -> Hashtbl.mem noeuds_actifs n.id) noeuds in

    (* suppresion des doublons*)
  let aretes_uniques = 
    let table = Hashtbl.create 15000 in
    List.filter (fun (a : arete) ->
      let cle = if a.source < a.destination then (a.source, a.destination, a.groupe) else (a.destination, a.source, a.groupe) in
      if Hashtbl.mem table cle then false else begin Hashtbl.add table cle true; true end
    ) arrete_visibles
  in

  (* creation de groupe*)
  let aretes_groupees = Hashtbl.create 15000 in
  List.iter (fun (a : arete) ->
    let cle = if a.source < a.destination then (a.source, a.destination) else (a.destination, a.source) in
    let existantes = try Hashtbl.find aretes_groupees cle with Not_found -> [] in
    Hashtbl.replace aretes_groupees cle (a :: existantes)
  ) aretes_uniques;

  (* appel du dessins sur chaque groupe*)
  Hashtbl.iter (fun _ groupe ->
    let n = List.length groupe in
    let groupe_trie = List.sort (fun a b -> String.compare a.groupe b.groupe) groupe in
    List.iteri (fun i a -> draw_arete etat.positions a i n) groupe_trie
  ) aretes_groupees;
  
  (*Dessin des noeuds *)
  List.iter (draw_noeud etat) noeuds_visibles

let redraw etat =
  Graphics.set_color Graphics.white; Graphics.fill_rect 0 0 largeur hauteur;
  draw_graph etat; graph_cache := Some (Graphics.get_image 0 0 largeur hauteur);
  draw_ui etat; Graphics.synchronize ()
  (* rafraichissement rapide utilisant l'image en cache.
   Au lieu de redessiner tous les primitifs, on blit l'image
   du graphe décalée de (dx, dy) puis on redessine les boutons.
   Utilisé pendant le déplacement (drag) pour de meilleures performances. *)
let redraw_cached dx dy etat =
  Graphics.set_color Graphics.white; Graphics.fill_rect 0 0 largeur hauteur;
  (match !graph_cache with Some img -> Graphics.draw_image img dx dy | None -> ());
  draw_ui etat; Graphics.synchronize ()

(*---------------FOCNTION INTERACTION----------------*)
(* inverse l'etat actif/inactif d'une categorie donnée par sont index i*)
let toggle categories i =
  List.mapi (fun j (c, v) -> (c, if i = j then not v else v)) categories
let is_in_rect mx my rx ry rw rh = (mx >= rx && mx <= (rx + rw) && my >= ry && my <= (ry + rh))

let traiter_clic mx my etat on_search =
  
  (* Raccourcis pour les calculs d'index *)
  let get_idx x_start gap = (mx - x_start) / (btn_w + gap) in

  (* on test (Chercher, Noms) *)
  if is_in_rect mx my (largeur - 120) (hauteur - 45) 100 35 then 
    ((match etat.source, etat.dest with 
      | Some s, Some d -> on_search s d etat.algo_actif etat.poids_actif 
      | _ -> print_endline "Sélectionnez une source et une destination !"); etat)

  else if is_in_rect mx my (largeur - 240) (hauteur - 45) 110 35 then 
    { etat with show_names = not etat.show_names }

  (* Ligne des catégories *)
  else if is_in_rect mx my 10 (hauteur - btn_h - 10) (List.length etat.categories * (btn_w + 10)) btn_h then
    { etat with categories = toggle etat.categories (get_idx 10 10) }

  (* Ligne des algos*)
  else if is_in_rect mx my 60 10 (List.length etat.algos * (btn_w + 5)) btn_h then
    { etat with algo_actif = List.nth etat.algos (get_idx 60 5) }

  (* Ligne des poids) *)
  else if etat.algo_actif = "Dijkstra" && is_in_rect mx my 60 50 (List.length etat.poids_list * (btn_w + 5)) btn_h then
    { etat with poids_actif = List.nth etat.poids_list (get_idx 60 5) }

  (*clique sur la carte*)
  else match get_noeud_clique mx my etat with
    | Some id ->
    let (new_s, new_d) =
          if Some id = etat.source then (None, None)               (*  Re-clic sur Source = On efface tout *)       
          else if Some id = etat.dest then (etat.source, None)     (*  Re-clic sur Dest = On efface juste la Dest *)
          else if etat.source = None then (Some id, None)          (*  Rien n'est coché = Devient la Source (Vert) *)
          else if etat.dest = None then (etat.source, Some id)     (*  Source déjà là = Devient la Dest (Rouge) *)
          else (Some id, None)                                     (*  Les deux sont là = On recommence avec une nouvelle Source *)
    in { etat with source = new_s; dest = new_d }
    | None -> etat

let rec drag etat debut_x debut_y =
  if not (Graphics.button_down ()) then
    let (mx, my) = Graphics.mouse_pos () in
    { etat with positions = move etat.positions (mx - debut_x) (my - debut_y) }
  else begin
    let (mx, my) = Graphics.mouse_pos () in
    if mx <> debut_x || my <> debut_y then redraw_cached (mx - debut_x) (my - debut_y) etat;
    drag etat debut_x debut_y
  end

(* boucle principale 
  attend un clique ou une touche et decide de quoi faire
  - clic sur bouton -> toggle categorie
  - clic ailleurs -> drag camera
  - touche + -> zoom in
  - touche - -> zoom out *)
let rec boucle etat on_search =
  let ev = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
  
  if ev.Graphics.keypressed then
    let zoom = if ev.Graphics.key = '+' || ev.Graphics.key = '=' then zoom_in_factor else zoom_out_factor in
    let (mx, my) = Graphics.mouse_pos () in
    let etat' = { etat with positions = zoom_positions etat.positions mx my zoom } in
    redraw etat'; boucle etat' on_search
  else
    let mx, my = ev.Graphics.mouse_x, ev.Graphics.mouse_y in
    let etat' = traiter_clic mx my etat on_search in
    if etat' = etat then
      let etat_drag = drag etat mx my in
      redraw etat_drag; boucle etat_drag on_search
    else begin
      redraw etat'; boucle etat' on_search
    end
(*----------------FONCTIONS PRINCIPALE------------------*)

let run noeuds aretes algos poids_list on_search =
  let bornes = calcule_bornes noeuds in
  let cats = List.sort_uniq String.compare (List.map (fun (a : arete) -> a.categorie) aretes) in
  
  let etat_initial = {
    noeuds; aretes;
    positions = calcule_positions bornes noeuds;
    categories = List.map (fun c -> (c, true)) cats;
    source = None; dest = None; show_names = false;
    algos; algo_actif = List.hd algos;
    poids_list; poids_actif = List.hd poids_list;
  } in

  Graphics.open_graph (" " ^ string_of_int largeur ^ "x" ^ string_of_int hauteur);
  Graphics.auto_synchronize false;
  redraw etat_initial;
  boucle etat_initial on_search