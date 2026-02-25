(* ------------------TYPE ET CONSTANTE------------------- *)

type noeud = { id : string; nom : string; x : float; y : float }

type arete = {
  source : string;
  destination : string;
  couleur : int;
  categorie : string;
  groupe : string;
}

type donnees_affichage = {
  noeuds : noeud list;
  aretes : arete list;
  positions : (string * (int * int)) list;
  categories : (string * bool) list;
  bornes : float * float * float * float;
}

type donnees_affichage = {noeuds : noeud list;aretes : arete list;positions : (string * (float * float)) list;categories : (string * bool) list;bornes : float * float * float * float;}
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
  match List.assoc_opt cle liste with Some valeur -> valeur | None -> defaut

(* Calcule les borne min/max de l'ensemble des noeuds 
  @param noeuds la liste des noeuds 
  return un tuple (min_x,max_x,min_y,max_y)*)
let calcule_bornes noeuds =
  let xs = List.map (fun n -> n.x) noeuds in
  let ys = List.map (fun n -> n.y) noeuds in
  ( List.fold_left min max_float xs,
    List.fold_left max min_float xs,
    List.fold_left min max_float ys,
    List.fold_left max min_float ys )

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

let get_noeud_clique mx my positions aretes categories =
  let arrete_visibles = List.filter (fun a -> cherche a.categorie categories true) aretes in
  let noeuds_actifs = Hashtbl.create 5000 in
  List.iter (fun a -> 
    Hashtbl.replace noeuds_actifs a.source true;
    Hashtbl.replace noeuds_actifs a.destination true
  ) arrete_visibles;
  
  let (best_id, _) =
    List.fold_left (fun (best, min_d) (id, (px, py)) ->
      let d = distance_carre px py mx my in
      if d <= 400.0 && d < min_d && Hashtbl.mem noeuds_actifs id then 
        (Some id, d) 
      else
        (best, min_d)
    ) (None, max_float) positions
  in
  best_id

(*---------------FONCTION DE DESSIN----------------*)

(* Cache du graphe rendu sous forme d'image.
   Permet d'éviter de redessiner tous les primitifs lors du déplacement. *)
let graph_cache : Graphics.image option ref = ref None

(* dessine une arrete (ligne) entre deux position donée
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param arete l'arrete à dessiner 
  @param i l'index de la ligne parmi les lignes parallèles
  @param n le nombre total de lignes parallèles *)
let draw_arete positions arete i n =
  let (x1, y1) = cherche arete.source positions (0., 0.) in
  let (x2, y2) = cherche arete.destination positions (0., 0.) in
  
  let (ox, oy) =
    if n <= 1 then (0., 0.) (* une station : on decalle pas*)
    else
      let (dx, dy) = 
        if arete.source < arete.destination then (x2 -. x1, y2 -. y1) (* comparaison pour decaller dans le meme sens*)
        else (x1 -. x2, y1 -. y2)
      in
      let len = sqrt (dx *. dx +. dy *. dy) in
      if len = 0. then (0., 0.)
      else
        let nx = -.dy /. len in
        let ny = dx /. len in
        let ecart = 4.0 in (* Espacement de 4 pixels entre chaque ligne *)
        let shift = (float i -. (float (n - 1) /. 2.0)) *. ecart in (* decalle pour centré le paquet*)
        (nx *. shift, ny *. shift)
  in

  Graphics.set_color arete.couleur;
  Graphics.set_line_width 3;
  Graphics.moveto (int_of_float (x1 +. ox)) (int_of_float (y1 +. oy));
  Graphics.lineto (int_of_float (x2 +. ox)) (int_of_float (y2 +. oy))

(* dessine une noeud et son nom à la position donnée
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param noeud le noeud à dessiner *)
let draw_noeud positions source dest show_names noeud =
  let (x, y) = cherche noeud.id positions (0., 0.) in
  let (r_ext, r_int) =
    if Some noeud.id = source then begin 
      Graphics.set_color Graphics.green;
      (9, 8)
    end
    else if Some noeud.id = dest then begin
      Graphics.set_color Graphics.red;
      (9, 8)
    end
    else begin
      Graphics.set_color Graphics.black;
      (5, 4)
    end
  in
  
  Graphics.fill_circle (int_of_float x) (int_of_float y) r_ext;
  Graphics.set_color Graphics.white;
  Graphics.fill_circle (int_of_float x) (int_of_float y) r_int;
  
  if show_names then begin
    Graphics.set_color Graphics.black;
    Graphics.moveto (int_of_float x + 12) (int_of_float y - 4);
    Graphics.draw_string (clean_accents noeud.nom)
  end

(* dessine un bouton pour une categorie(vert si actif gris sinon)
  @param i l'index du bouton
  @param (nom, actif) le nom et l'état du bouton *)
let draw_bouton i (nom, actif) =
  let x = 10 + (i * (btn_w + 10)) in
  let y = hauteur - btn_h - 10 in
  Graphics.set_color (if actif then 0x4CAF50 else 0xCCCCCC);
  Graphics.fill_rect x y btn_w btn_h;
  Graphics.set_color Graphics.black;
  Graphics.draw_rect x y btn_w btn_h;
  Graphics.moveto (x + 10) (y + 8);
  Graphics.draw_string (clean_accents nom)

let draw_search_btn () =
  let x = largeur - 120 and y = hauteur - 45 in
  Graphics.set_color Graphics.blue; Graphics.fill_rect x y 100 35;
  Graphics.set_color Graphics.white; Graphics.moveto (x + 15) (y + 12);
  Graphics.draw_string "CHERCHER"

let is_search_clicked mx my =
  mx >= largeur - 120 && mx <= largeur - 20 && my >= hauteur - 45 && my <= hauteur - 10

let draw_names_btn show_names =
  let x = largeur - 240 and y = hauteur - 45 in
  Graphics.set_color (if show_names then 0x4CAF50 else 0xCCCCCC);
  Graphics.fill_rect x y 110 35;
  Graphics.set_color Graphics.black;
  Graphics.draw_rect x y 110 35;
  Graphics.moveto (x + 10) (y + 12);
  Graphics.draw_string (if show_names then "NOMS : ON" else "NOMS : OFF")

let is_names_clicked mx my =
  mx >= largeur - 240 && mx <= largeur - 130 && my >= hauteur - 45 && my <= hauteur - 10

(* dessine uniquement le graphe (aretes + noeuds), sans effacer ni synchroniser.
   Utilisé en interne par redraw.
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param noeuds la liste des noeuds
  @param aretes la liste des aretes
  @param categories la liste des categories (pour filtrer) *)
let draw_graph positions noeuds aretes categories source dest show_names =
  let arrete_visibles = List.filter (fun a -> cherche a.categorie categories true) aretes in
  
  let noeuds_actifs = Hashtbl.create 15000 in
  List.iter (fun a -> 
    Hashtbl.replace noeuds_actifs a.source true;
    Hashtbl.replace noeuds_actifs a.destination true
  ) arrete_visibles;

  let noeuds_visibles = List.filter (fun n -> Hashtbl.mem noeuds_actifs n.id) noeuds in

    (* suppresion des doublons*)
  let aretes_uniques = 
    let table = Hashtbl.create 15000 in
    List.filter (fun a ->
      let cle = if a.source < a.destination then (a.source, a.destination, a.groupe) else (a.destination, a.source, a.groupe) in
      if Hashtbl.mem table cle then false else begin Hashtbl.add table cle true; true end
    ) arrete_visibles
  in

  (* creation de groupe*)
  let aretes_groupees = Hashtbl.create 15000 in
  List.iter (fun a ->
    let cle = if a.source < a.destination then (a.source, a.destination) else (a.destination, a.source) in
    let existantes = try Hashtbl.find aretes_groupees cle with Not_found -> [] in
    Hashtbl.replace aretes_groupees cle (a :: existantes)
  ) aretes_uniques;

  (* appel du dessins sur chaque groupe*)
  Hashtbl.iter (fun _ groupe ->
    let n = List.length groupe in
    let groupe_trie = List.sort (fun a b -> String.compare a.groupe b.groupe) groupe in
    List.iteri (fun i a -> draw_arete positions a i n) groupe_trie
  ) aretes_groupees;
  
  List.iter (draw_noeud positions source dest show_names) noeuds_visibles

(* Capture l'écran actuel (graphe sans boutons) dans le cache image *)
let cache_graph () =
  graph_cache := Some (Graphics.get_image 0 0 largeur hauteur)

(* fonction de rafraichissement complet de l'ecran
  1. efface l'ecran
  2. dessine le graphe (aretes + noeuds)
  3. met en cache l'image du graphe
  4. dessine les boutons par dessus
  5. synchronise l'affichage
  
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param noeuds la liste des noeuds
  @param aretes la liste des aretes
  @param categories la liste des categories (pour filtrer)*)
let redraw positions noeuds aretes categories source dest show_names =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 largeur hauteur;
  draw_graph positions noeuds aretes categories source dest show_names;
  cache_graph ();
  List.iteri draw_bouton categories;
  draw_search_btn ();
  draw_names_btn show_names;
  Graphics.synchronize ()

(* rafraichissement rapide utilisant l'image en cache.
   Au lieu de redessiner tous les primitifs, on blit l'image
   du graphe décalée de (dx, dy) puis on redessine les boutons.
   Utilisé pendant le déplacement (drag) pour de meilleures performances.
  @param dx décalage horizontal depuis le début du drag
  @param dy décalage vertical depuis le début du drag
  @param categories la liste des categories (pour les boutons) *)
let redraw_cached dx dy categories show_names =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 largeur hauteur;
  (match !graph_cache with
  | Some img -> Graphics.draw_image img dx dy
  | None -> ());
  List.iteri draw_bouton categories;
  draw_search_btn ();
  draw_names_btn show_names;
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
  Utilise l'image en cache pour afficher le graphe décalé (rapide).
  Les positions ne sont mises à jour qu'une seule fois, à la fin du drag.
  
  @param positions les positions de départ (au début du drag)
  @param noeuds la liste des noeuds
  @param aretes la liste des aretes
  @param categories la liste des categories (pour filtrer)
  @param debut_x la coordonnée x du début du drag
  @param debut_y la coordonnée y du début du drag *)
let rec drag positions noeuds aretes categories source dest show_names debut_x debut_y =
  if not (Graphics.button_down ()) then begin
    (* Drag terminé : on calcule le décalage final, on met à jour
       les positions et on fait un redraw complet pour recacher. *)
    let mx, my = Graphics.mouse_pos () in
    let dx = mx - debut_x in
    let dy = my - debut_y in
    let nouvelles_pos = move positions dx dy in
    redraw nouvelles_pos noeuds aretes categories source dest show_names;
    nouvelles_pos
  end
  else
    let mx, my = Graphics.mouse_pos () in
    let dx = mx - debut_x in
    let dy = my - debut_y in
    if dx = 0 && dy = 0 then
      drag positions noeuds aretes categories source dest show_names debut_x debut_y
    else begin
      (* Pendant le drag : on blit l'image en cache décalée (rapide) *)
      redraw_cached dx dy categories show_names;
      drag positions noeuds aretes categories source dest show_names debut_x debut_y
    end

(* boucle principale 
  attend un clique ou une touche et decide de quoi faire
  - clic sur bouton -> toggle categorie
  - clic ailleurs -> drag camera
  - touche + -> zoom in
  - touche - -> zoom out *)
let rec boucle positions noeuds aretes categories source dest show_names on_search =
  let ev = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
  
  (* Gestion du zoom clavier *)
  if ev.Graphics.keypressed then begin
    let mx, my = Graphics.mouse_pos () in
    let key = ev.Graphics.key in
    let zoom = if key = '+' ||key = '=' then zoom_in_factor else zoom_out_factor in
      let positions' = zoom_positions positions mx my zoom in
      redraw positions' noeuds aretes categories source dest show_names;
      boucle positions' noeuds aretes categories source dest show_names on_search
  end
  else begin
    (* Gestion des clics *)
    let mx = ev.Graphics.mouse_x in
    let my = ev.Graphics.mouse_y in
    
    if is_search_clicked mx my then begin
      (match source, dest with
       | Some s, Some d -> on_search s d
       | _ -> print_endline "Sélectionnez d'abord une source et une destination !");
      boucle positions noeuds aretes categories source dest show_names on_search
    end else if is_names_clicked mx my then begin
      let new_show_names = not show_names in
      redraw positions noeuds aretes categories source dest new_show_names;
      boucle positions noeuds aretes categories source dest new_show_names on_search
    end else
    
    match clic_bouton mx my (List.length categories) with
    | Some i -> 
        let categories = toggle categories i in
        redraw positions noeuds aretes categories source dest show_names; 
        boucle positions noeuds aretes categories source dest show_names on_search
    | None -> 
        match get_noeud_clique mx my positions aretes categories with
        | Some id ->
            let (new_s, new_d) =
              if Some id = source then (None, None)          (*  Re-clic sur Source = On efface tout *)
              else if Some id = dest then (source, None)     (*  Re-clic sur Dest = On efface juste la Dest *)
              else if source = None then (Some id, None)     (*  Rien n'est coché = Devient la Source (Vert) *)
              else if dest = None then (source, Some id)     (*  Source déjà là = Devient la Dest (Rouge) *)
              else (Some id, None)                           (*  Les deux sont là = On recommence avec une nouvelle Source *)
            in
            redraw positions noeuds aretes categories new_s new_d show_names;
            boucle positions noeuds aretes categories new_s new_d show_names on_search
        | None ->
            (* Cas 2 : Clic dans le vide -> On lance la fonction drag *)
            let positions_bis = drag positions noeuds aretes categories source dest show_names mx my in
            boucle positions_bis noeuds aretes categories source dest show_names on_search
  end
(*----------------FONCTIONS PRINCIPALE------------------*)

(*main*)
let run noeuds aretes on_search =
  let bornes = calcule_bornes noeuds in
  let positions = calcule_positions bornes noeuds in
  let cats = List.map (fun a -> a.categorie) aretes in
  let categories =
    List.map (fun c -> (c, true)) (List.sort_uniq String.compare cats)
  in
  Graphics.open_graph (" " ^ string_of_int largeur ^ "x" ^ string_of_int hauteur);
  Graphics.auto_synchronize false;
  let show_names = false in
  redraw positions noeuds aretes categories None None show_names;
  boucle positions noeuds aretes categories None None show_names on_search