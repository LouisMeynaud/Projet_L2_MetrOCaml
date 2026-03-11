(* ------------------TYPE ET CONSTANTE------------------- *)
open Bogue

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
    (float (!largeur - 2*marge) /. (max_x -. min_x)) 
                    (float (!hauteur - 2*marge) /. (max_y -. min_y)) in
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



let mon_canvas = ref None
let get_canvas () = match !mon_canvas with Some c -> c | None -> failwith "Canvas non initialisé"


let hex_to_rgb hexa = 
  let rouge = (hexa lsr 16) land 255 in (* lsr : décalage vers la droite et land coupe les deux derniers bits *)
  let vert  = (hexa lsr 8) land 255 in
  let bleu  = hexa land 255 in
  (rouge, vert, bleu)

let rgb_to_bogue (r, g, b) = Draw.opaque (r, g, b)


let police_actuelle = ref None

let charger_police () =
  if !police_actuelle = None then begin
    let liste_candidats = ["FreeSans.otf"; "LiberationSans-Regular.ttf"; "DejaVuSans.ttf"] in
    
    (* On cherche la première police qui existe sur l'ordinateur *)
    let chemin_trouve = 
      List.fold_left (fun resultat_precedent nom_police -> 
        match resultat_precedent with 
        | Some _ -> resultat_precedent (* On a déjà trouvé, on garde *)
        | None -> Theme.get_font_path_opt nom_police (* On essaie la suivante *)
      ) None liste_candidats 
    in
    
    (* Si on a trouvé un chemin, on demande à SDL d'ouvrir la police taille 12 *)
    match chemin_trouve with 
    | Some chemin -> 
        (match Tsdl_ttf.Ttf.open_font chemin 12 with 
         | Ok vraie_police -> police_actuelle := Some vraie_police 
         | Error _ -> ()) 
    | None -> ()
  end

(* 3. Le dessin du texte *)
let dessiner_texte x y texte (r, g, b) =
  charger_police ();
  
  match !police_actuelle with
  | None -> () (* Si on n'a pas de police, on abandonne *)
  | Some ma_police ->
      Sdl_area.add (get_canvas ()) (fun pinceau_sdl ->
        let couleur_texte = Tsdl.Sdl.Color.create ~r ~g ~b ~a:255 in
        
        (*  Dessiner le texte sur une image en mémoire (Surface) *)
        match Tsdl_ttf.Ttf.render_utf8_blended ma_police texte couleur_texte with
        | Error _ -> ()
        | Ok image_texte ->
            
            (* Envoyer l'image à la carte graphique (Texture) *)
            (match Tsdl.Sdl.create_texture_from_surface pinceau_sdl image_texte with
             | Error _ -> ()
             | Ok image_carte_graphique ->
                 
                 (*  Récupérer la taille du texte et le coller à l'écran *)
                 (match Tsdl.Sdl.query_texture image_carte_graphique with
                  | Error _ -> ()
                  | Ok (_, _, (largeur, hauteur)) -> 
                      let zone_ecran = Tsdl.Sdl.Rect.create ~x ~y ~w:largeur ~h:hauteur in
                      let _ = Tsdl.Sdl.render_copy ~dst:zone_ecran pinceau_sdl image_carte_graphique in
                      ()
                 );
                 
                 (*  Nettoyer la mémoire de la carte graphique *)
                 Tsdl.Sdl.destroy_texture image_carte_graphique
            );
            
            (*  Nettoyer la mémoire de l'ordinateur *)
            Tsdl.Sdl.free_surface image_texte
      )

(*---------------FONCTION DE DESSIN----------------*)

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
  let color = rgb_to_bogue (hex_to_rgb arete.couleur) in
  Sdl_area.draw_line (get_canvas ()) ~color ~thick:3
    (int_of_float (x1 +. ox), int_of_float (y1 +. oy))
    (int_of_float (x2 +. ox), int_of_float (y2 +. oy))

(*dessine un noeud*)
let draw_noeud etat noeud =
  let (x, y) = cherche noeud.id etat.positions (0., 0.) in
  let (color_ext, r_ext, r_int) =
    if Some noeud.id = etat.source then (hex_to_rgb 0x00FF00, 9, 8)
    else if Some noeud.id = etat.dest then (hex_to_rgb 0xFF0000, 9, 8)
    else (hex_to_rgb 0x000000, 5, 4)
  in
  Sdl_area.fill_circle (get_canvas ()) ~color:(rgb_to_bogue color_ext) ~radius:r_ext (int_of_float x, int_of_float y);
  Sdl_area.fill_circle (get_canvas ()) ~color:(rgb_to_bogue (255,255,255)) ~radius:r_int (int_of_float x, int_of_float y);
  if etat.show_names then begin
    dessiner_texte (int_of_float x + 12) (int_of_float y - 4) (clean_accents noeud.nom) (0,0,0)
  end

(* dessine un bouton *)
let draw_bouton x y w h text (r,g,b) (fond_r,fond_g,fond_b) =
  Sdl_area.fill_rectangle (get_canvas ()) ~color:(rgb_to_bogue (fond_r,fond_g,fond_b)) ~w ~h (x, y);
  Sdl_area.draw_rectangle (get_canvas ()) ~color:(rgb_to_bogue (r,g,b)) ~thick:1 ~w ~h (x, y);
  dessiner_texte (x + 10) (y + 8) (clean_accents text) (r,g,b)

let draw_selection_row y_pos label boutons actif =
  dessiner_texte 5 (y_pos + 8) label (0,0,0);
  List.iteri (fun i nom -> 
    let couleur_fond = if nom = actif then hex_to_rgb 0x2196F3 else hex_to_rgb 0xCCCCCC in
    draw_bouton (60 + i * (btn_w + 5)) y_pos btn_w btn_h nom (0,0,0) couleur_fond
  ) boutons

let draw_ui etat =
  List.iteri (fun i (nom, actif) -> 
    let couleur_fond = if actif then hex_to_rgb 0x4CAF50 else hex_to_rgb 0xCCCCCC in
    draw_bouton (10 + i * (btn_w + 10)) (!hauteur - btn_h - 10) btn_w btn_h nom (0,0,0) couleur_fond
  ) etat.categories;
  draw_bouton (!largeur - 120) (!hauteur - 45) 100 35 "CHERCHER" (255,255,255) (hex_to_rgb 0x0000FF);
  
  let col_noms = if etat.show_names then hex_to_rgb 0x4CAF50 else hex_to_rgb 0xCCCCCC in
  draw_bouton (!largeur - 240) (!hauteur - 45) 110 35 (if etat.show_names then "NOMS : ON" else "NOMS : OFF") (0,0,0) col_noms;
  
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

let cache_photo = ref None

let rafraichir_ecran etat =
  Sdl_area.clear (get_canvas ());
  Sdl_area.fill_rectangle (get_canvas ()) ~color:(rgb_to_bogue (255,255,255)) ~w:!largeur ~h:!hauteur (0,0);
  draw_graph etat;
  draw_ui etat;
  
  (* Mise en mémoire de la photo de ce qu'on vient de dessiner *)
  (match Sdl_area.get_texture (get_canvas ()) with
   | Some tex -> cache_photo := Some tex
   | None -> ());
   
  Sdl_area.update (get_canvas ())

(*---------------FOCNTION INTERACTION----------------*)
(* inverse l'etat actif/inactif d'une categorie donnée par sont index i*)
let toggle categories i =
  List.mapi (fun j (c, v) -> (c, if i = j then not v else v)) categories
let is_in_rect mx my rx ry rw rh = (mx >= rx && mx <= (rx + rw) && my >= ry && my <= (ry + rh))

let traiter_clic mx my etat on_search =
  
  (* Raccourcis pour les calculs d'index *)
  let get_idx x_start gap = (mx - x_start) / (btn_w + gap) in

  (* on test (Chercher, Noms) *)
  if is_in_rect mx my (!largeur - 120) (!hauteur - 45) 100 35 then 
    ((match etat.source, etat.dest with 
      | Some s, Some d -> on_search s d etat.algo_actif etat.poids_actif 
      | _ -> print_endline "Sélectionnez une source et une destination !"); etat)

  else if is_in_rect mx my (!largeur - 240) (!hauteur - 45) 110 35 then 
    { etat with show_names = not etat.show_names }

  (* Ligne des catégories *)
  else if is_in_rect mx my 10 (!hauteur - btn_h - 10) (List.length etat.categories * (btn_w + 10)) btn_h then
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

(*----------------FONCTIONS PRINCIPALE------------------*)
let run noeuds aretes algos poids_list on_search =
  largeur := 1200;
  hauteur := 800;

  let bornes = calcule_bornes noeuds in
  let cats = List.sort_uniq String.compare (List.map (fun (a : arete) -> a.categorie) aretes) in

  let widget_dessin = Widget.sdl_area ~w:!largeur ~h:!hauteur () in
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
  let is_dragging = ref false in
  let debut_x = ref 0 in
  let debut_y = ref 0 in
  let pos_initiales = ref [] in
  let needs_redraw = ref false in
  let action_courante = ref "Rien" (* Pour savoir si on doit redessiner tout ou juste glisser l'image *)

  let boucle _w _target ev =
    let etat = !current_etat in

    match Trigger.event_kind ev with
    
    | `Key_down ->
      let key = Tsdl.Sdl.Event.(get ev keyboard_keycode) in
      let open Tsdl.Sdl.K in
      let zoom = if key = plus || key = equals || key = kp_plus then zoom_in_factor else zoom_out_factor in
      if zoom <> 1.0 then begin
        let mx, my = !largeur / 2, !hauteur / 2 in
        let etat' = { etat with positions = zoom_positions etat.positions mx my zoom } in
        current_etat := etat';
        action_courante := "Zoom";
        needs_redraw := true
      end

    | `Mouse_button_down | `Finger_down ->
      let mx, my = Sdl_area.pointer_pos (get_canvas ()) ev in
      let etat' = traiter_clic mx my etat on_search in
      
      if etat' = etat then begin
        is_dragging := true;
        debut_x := mx;
        debut_y := my;
        pos_initiales := etat.positions
      end else begin
        current_etat := etat';
        action_courante := "Clic";
        needs_redraw := true
      end

    | `Mouse_button_up | `Finger_up ->
      if !is_dragging then begin
         is_dragging := false;
         action_courante := "FinDrag";
         needs_redraw := true (* On lâche la carte, on redessine proprement *)
      end

    | `Mouse_motion | `Finger_motion ->
      if !is_dragging then begin
        let mx, my = Sdl_area.pointer_pos (get_canvas ()) ev in
        current_etat := { etat with positions = move !pos_initiales (mx - !debut_x) (my - !debut_y) };
        action_courante := "Drag";
        needs_redraw := true 
      end

    | _ -> ()
  in

  let liste_ev = Trigger.buttons_down @ Trigger.buttons_up @ Trigger.pointer_motion @ [Trigger.key_down] in
  let connexion = Widget.connect_main widget_dessin widget_dessin boucle liste_ev in

  let mise_en_page = Layout.resident ~w:!largeur ~h:!hauteur widget_dessin in
  let fenetre = Main.of_layout ~connections:[connexion] mise_en_page in

  rafraichir_ecran etat_initial;

  let rafraichissement_continu () =
    if !needs_redraw then begin
      needs_redraw := false;
      if !action_courante = "Drag" && !cache_photo <> None then begin
        (* Si on glisse, on efface tout et on colle juste la photo avec le décalage *)
        Sdl_area.clear (get_canvas ());
        Sdl_area.fill_rectangle (get_canvas ()) ~color:(rgb_to_bogue (255,255,255)) ~w:!largeur ~h:!hauteur (0,0);
        Sdl_area.add (get_canvas ()) (fun renderer -> 
           let (mx, my) = Tsdl.Sdl.get_mouse_state () in
           let dx = mx - !debut_x and dy = my - !debut_y in
           ignore (Tsdl.Sdl.render_copy ~dst:(Tsdl.Sdl.Rect.create ~x:dx ~y:dy ~w:!largeur ~h:!hauteur) renderer (Option.get !cache_photo))
        );
        draw_ui !current_etat;
        Sdl_area.update (get_canvas ())
      end else begin
        (* Sinon (zoom, fin du drag, clic...), on refait le calcul complet *)
        rafraichir_ecran !current_etat
      end
    end
  in

  Main.run ~before_display:rafraichissement_continu fenetre