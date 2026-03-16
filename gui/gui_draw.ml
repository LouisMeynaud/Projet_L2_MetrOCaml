(* gui_draw.ml *)
open Bogue
open Gui_types

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
    (float (!canvas_w - 2*marge) /. (max_x -. min_x)) 
                    (float (!canvas_h - 2*marge) /. (max_y -. min_y)) in
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
    dessiner_texte (int_of_float x + 12) (int_of_float y - 4) noeud.nom (0,0,0)
  end

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

  (* rafraichit l'écran en dessinant le graphe *)
let cache_photo = ref None
let cible_originale = ref None
let cache_w = ref 0 (* largeur de la "photo" du graphe *)
let cache_h = ref 0 (* hauteur de la "photo" du graphe *)

(* détruit la texture de cache si elle existe *)
let destroy_cache_texture () =
  match !cache_photo with
  | Some t ->
      Tsdl.Sdl.destroy_texture t;
      cache_photo := None;
      cache_w := 0;
      cache_h := 0
  | None -> ()

  (* rafraichit l'écran en dessinant le graphe, en utilisant une   "photo" pour éviter de redessiner tout le graphe pendant les glissements *)
let rafraichir_ecran etat =
  (* Si on affiche les noms, on ne peut pas utiliser la "photo" du graphe car les positions des textes changent à chaque zoom ou déplacement (et le texte deviendrai flou), donc on redessine tout à chaque fois *)
  if etat.show_names then begin
    destroy_cache_texture ();
    Sdl_area.clear (get_canvas ());
    draw_graph etat;
    Sdl_area.update (get_canvas ())
  end else begin
    (* Si on n'affiche pas les noms, on peut utiliser la "photo" du graphe pour faire un glissement fluide sans redessiner tout le graphe à chaque fois *)
  Sdl_area.clear (get_canvas ());

  (*On prépare une image invisible et on dit à Bogue de dessiner dessus *)
  Sdl_area.add (get_canvas ()) (fun renderer ->
    cible_originale := Tsdl.Sdl.get_render_target renderer; (* On sauvegarde l'écran normal *)

    
    let wanted_w = max 1 !canvas_w in
    let wanted_h = max 1 !canvas_h in
    let must_recreate =
      (* Vérifie si la texture de cache doit être recréée *)
      match !cache_photo with
      | None -> true
      | Some _ -> !cache_w <> wanted_w || !cache_h <> wanted_h
    in

    (* si doit etre redessinée , on la recrée *)
    if must_recreate then begin
      destroy_cache_texture ();
      match Tsdl.Sdl.create_texture renderer Tsdl.Sdl.Pixel.format_argb8888 Tsdl.Sdl.Texture.access_target ~w:wanted_w ~h:wanted_h with
      | Ok texture ->
          cache_photo := Some texture;
          cache_w := wanted_w;
          cache_h := wanted_h
      | Error _ -> ()
    end;

    match !cache_photo with
    | Some texture ->
        ignore (Tsdl.Sdl.set_render_target renderer (Some texture)); 
        ignore (Tsdl.Sdl.set_render_draw_color renderer 255 255 255 255);
        ignore (Tsdl.Sdl.render_clear renderer)
    | None -> ()
  );

  draw_graph etat;
  Sdl_area.add (get_canvas ()) (fun renderer ->
    ignore (Tsdl.Sdl.set_render_target renderer !cible_originale);
    match !cache_photo with
    | Some tex -> ignore (Tsdl.Sdl.render_copy renderer tex)
    | None -> ()
  );

  Sdl_area.update (get_canvas ())
  end


(* Utilisé uniquement pendant qu'on glisse la souris *)
let rafraichir dx dy etat =
  Sdl_area.clear (get_canvas ());
  Sdl_area.add (get_canvas ()) (fun renderer ->
    ignore (Tsdl.Sdl.set_render_draw_color renderer 255 255 255 255); 
    ignore (Tsdl.Sdl.render_fill_rect renderer None);
    
    (* On déplace la "photo" du graphe de dx et dy *)
    match !cache_photo with 
    | None -> () 
    | Some tex ->
      let w = if !cache_w > 0 then !cache_w else !canvas_w in
      let h = if !cache_h > 0 then !cache_h else !canvas_h in
      ignore (Tsdl.Sdl.render_copy ~dst:(Tsdl.Sdl.Rect.create ~x:dx ~y:dy ~w ~h) renderer tex)
  );
  
  Sdl_area.update (get_canvas ())

