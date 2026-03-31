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
  let dx = max 1e-9 (max_x -. min_x) in
  let dy = max 1e-9 (max_y -. min_y) in
  let echelle = min 
    (float (max 1 (!canvas_w - 2*marge)) /. dx)
    (float (max 1 (!canvas_h - 2*marge)) /. dy) in
  (float marge +. (x -. min_x) *. echelle,
   float marge +. (max_y -. y) *. echelle)

(*calcule les position initiales pour tout les noeud
  return une linste associative (id,(x_pixel,y_pixel));..
  @param bornes les bornes de l'ensemble des noeuds
  @param noeuds la liste des noeuds*)
let calcule_positions bornes noeuds =
  let positions = Hashtbl.create (max 1024 (List.length noeuds * 2)) in
  List.iter (fun n -> Hashtbl.replace positions n.id (to_screen bornes n.x n.y)) noeuds;
  positions




let distance_carre px py mx my =
  let dx = px -. float mx in
  let dy = py -. float my in
  dx *. dx +. dy *. dy

let get_noeud_clique mx my etat =
  let (best_id, _) = Hashtbl.fold (fun id (wx, wy) (best, min_d) ->
      let px = wx *. etat.zoom +. etat.camera_x in
      let py = wy *. etat.zoom +. etat.camera_y in
      let d = distance_carre px py mx my in
      if d <= 400.0 && d < min_d && Hashtbl.mem etat.noeuds_actifs id then (Some id, d) else (best, min_d)
    ) etat.positions (None, max_float)
  in best_id

let rebuild_render etat =

  let arrete_visibles =
    List.filter (fun (a : arete) -> cherche a.categorie etat.categories true) etat.aretes
  in

  let aretes_uniques =
    let table = Hashtbl.create 15000 in
    List.filter (fun (a : arete) ->
      let cle =
        if a.source < a.destination
        then (a.source, a.destination, a.groupe)
        else (a.destination, a.source, a.groupe)
      in
      if Hashtbl.mem table cle then false
      else begin
        Hashtbl.add table cle true;
        true
      end
    ) arrete_visibles
  in

  let aretes_groupees = Hashtbl.create 15000 in
  let noeuds_actifs = Hashtbl.create 15000 in
  List.iter (fun (a : arete) ->
    let cle =
      if a.source < a.destination
      then (a.source, a.destination)
      else (a.destination, a.source)
    in
    let existantes = match Hashtbl.find_opt aretes_groupees cle with Some l -> l | None -> [] in
    Hashtbl.replace aretes_groupees cle (a :: existantes);
    Hashtbl.replace noeuds_actifs a.source true;
    Hashtbl.replace noeuds_actifs a.destination true
  ) aretes_uniques;

  Hashtbl.iter (fun cle groupe ->
    let groupe_trie = List.sort (fun a b -> String.compare a.groupe b.groupe) groupe in
    Hashtbl.replace aretes_groupees cle groupe_trie
  ) aretes_groupees;

  let noeuds_visibles = List.filter (fun n -> Hashtbl.mem noeuds_actifs n.id) etat.noeuds in

  { etat with aretes_groupees; noeuds_visibles; noeuds_actifs }



let mon_canvas = ref None
let get_canvas () =
  match !mon_canvas with Some c -> c | None -> failwith "Canvas non initialisé"


let hex_to_rgb hexa = 
  let rouge = (hexa lsr 16) land 255 in (* lsr : décalage vers la droite et land coupe les deux derniers bits *)
  let vert  = (hexa lsr 8) land 255 in
  let bleu  = hexa land 255 in
  (rouge, vert, bleu)

let rgb_to_bogue (r, g, b) =
  Draw.opaque (r, g, b)


  let police_actuelle = ref None
  let police_tente = ref false

type label_cache_entry = {
  texture : Tsdl.Sdl.texture;
  largeur : int;
  hauteur : int;
}

let label_cache : ((string * int * int * int), label_cache_entry) Hashtbl.t =
  Hashtbl.create 2048

let clear_label_cache () =
  Hashtbl.iter (fun _ entry -> Tsdl.Sdl.destroy_texture entry.texture) label_cache;
  Hashtbl.clear label_cache

  let charger_police () =
    if not !police_tente then begin
      police_tente := true;
      if Tsdl_ttf.Ttf.was_init () = true then ignore (Tsdl_ttf.Ttf.init ());
  
      let chemins = ["/System/Library/Fonts/Helvetica.ttc"; 
      "/Library/Fonts/Arial.ttf"; 
      "C:\\Windows\\Fonts\\arial.ttf"; 
      "C:/Windows/Fonts/arial.ttf"; 
      "/Windows/Fonts/arial.ttf"; 
      "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"; 
      "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"] in
      let rec trouver_police = function
        | [] -> Printf.printf "Aucune police système trouvée.\n%!"
        | chemin :: suite ->
            if Sys.file_exists chemin then
              match Tsdl_ttf.Ttf.open_font chemin 16 with
              | Ok p -> police_actuelle := Some p
              | Error _ -> trouver_police suite
            else trouver_police suite
      in
      trouver_police chemins
    end

(* 3. Le dessin du texte *)
let dessiner_texte x y texte (r, g, b) =
  charger_police ();
  
  match !police_actuelle with
  | None -> () (* Si on n'a pas de police, on abandonne *)
  | Some ma_police ->
      Sdl_area.add (get_canvas ()) (fun pinceau_sdl ->
    let key = (texte, r, g, b) in
    let draw_entry entry =
      let zone_ecran = Tsdl.Sdl.Rect.create ~x ~y ~w:entry.largeur ~h:entry.hauteur in
      let _ = Tsdl.Sdl.render_copy ~dst:zone_ecran pinceau_sdl entry.texture in
      ()
    in
    match Hashtbl.find_opt label_cache key with
    | Some entry -> draw_entry entry
    | None ->
      let couleur_texte = Tsdl.Sdl.Color.create ~r ~g ~b ~a:255 in
      match Tsdl_ttf.Ttf.render_utf8_blended ma_police texte couleur_texte with
      | Error _ -> ()
      | Ok image_texte ->
        (match Tsdl.Sdl.create_texture_from_surface pinceau_sdl image_texte with
         | Error _ -> Tsdl.Sdl.free_surface image_texte
         | Ok texture ->
           (match Tsdl.Sdl.query_texture texture with
            | Error _ ->
              Tsdl.Sdl.destroy_texture texture;
              Tsdl.Sdl.free_surface image_texte
            | Ok (_, _, (largeur, hauteur)) ->
              let entry = { texture; largeur; hauteur } in
              Hashtbl.add label_cache key entry;
              Tsdl.Sdl.free_surface image_texte;
              draw_entry entry)
        )
      )

(*---------------FONCTION DE DESSIN----------------*)

(* dessine une arrete (ligne) entre deux position donée
  @param positions la liste associative (id,(x_pixel,y_pixel));..
  @param arete l'arrete à dessiner 
  @param i l'index de la ligne parmi les lignes parallèles
  @param n le nombre total de lignes parallèles *)
  let draw_arete etat (arete : arete) i n =

    let get_pos id =
      match Hashtbl.find_opt etat.positions id with
      | Some p -> p 
      | None -> (0., 0.)
    in
    let (wx1, wy1) = get_pos arete.source in
    let (wx2, wy2) = get_pos arete.destination in
    let x1 = wx1 *. etat.zoom +. etat.camera_x in
    let y1 = wy1 *. etat.zoom +. etat.camera_y in
    let x2 = wx2 *. etat.zoom +. etat.camera_x in
    let y2 = wy2 *. etat.zoom +. etat.camera_y in
  
    (* on ignore ce qui est hors de l'écran *)
    let pad = 32.0 in
    let min_x = -.pad and min_y = -.pad in
    let max_x = float !canvas_w +. pad and max_y = float !canvas_h +. pad in
  
    if (x1 < min_x && x2 < min_x) || (x1 > max_x && x2 > max_x) ||
       (y1 < min_y && y2 < min_y) || (y1 > max_y && y2 > max_y) then
    () (* On ne dessine rien *)
    else
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
      let clamp_int_coord f =
        if Float.is_nan f || Float.is_infinite f then 0
        else max (-50000) (min 50000 (int_of_float f))
      in
      let x1i = clamp_int_coord (x1 +. ox) in
      let y1i = clamp_int_coord (y1 +. oy) in
      let x2i = clamp_int_coord (x2 +. ox) in
      let y2i = clamp_int_coord (y2 +. oy) in
      if x1i = x2i && y1i = y2i then 
        ()
      else 
        let r, g, b = hex_to_rgb arete.couleur in
        Sdl_area.add (get_canvas ()) (fun renderer ->
          ignore (Tsdl.Sdl.set_render_draw_color renderer r g b 255);
          ignore (Tsdl.Sdl.render_draw_line renderer x1i y1i x2i y2i);
          ignore (Tsdl.Sdl.render_draw_line renderer (x1i+1) y1i (x2i+1) y2i);
          ignore (Tsdl.Sdl.render_draw_line renderer (x1i-1) y1i (x2i-1) y2i);
          ignore (Tsdl.Sdl.render_draw_line renderer x1i (y1i+1) x2i (y2i+1));
          ignore (Tsdl.Sdl.render_draw_line renderer x1i (y1i-1) x2i (y2i-1))
        )

(*dessine un noeud*)
let draw_noeud etat noeud =
  let (wx, wy) = 
  match Hashtbl.find_opt etat.positions noeud.id with 
  | Some p -> p 
  | None -> (0., 0.) 
in
  let x = wx *. etat.zoom +. etat.camera_x in
  let y = wy *. etat.zoom +. etat.camera_y in

  let pad = 20.0 in
  if x < -.pad || y < -.pad || x > float !canvas_w +. pad || y > float !canvas_h +. pad then
    ()
  else
    let (color_ext, r_ext, r_int) =
      if Some noeud.id = etat.source then (hex_to_rgb 0x00FF00, 11, 9)
      else if Some noeud.id = etat.dest then (hex_to_rgb 0xFF0000, 11, 9)
      else (hex_to_rgb 0x000000, 5,3)
    in
    
    let r, g, b = color_ext in
    let cx = int_of_float x in
    let cy = int_of_float y in
    
    Sdl_area.add (get_canvas ()) (fun renderer ->
      ignore (Tsdl.Sdl.set_render_draw_color renderer r g b 255);
      let rect_ext = Tsdl.Sdl.Rect.create ~x:(cx - r_ext) ~y:(cy - r_ext) ~w:(r_ext * 2) ~h:(r_ext * 2) in
      ignore (Tsdl.Sdl.render_fill_rect renderer (Some rect_ext));
      
      ignore (Tsdl.Sdl.set_render_draw_color renderer 255 255 255 255);
      let rect_int = Tsdl.Sdl.Rect.create ~x:(cx - r_int) ~y:(cy - r_int) ~w:(r_int * 2) ~h:(r_int * 2) in
      ignore (Tsdl.Sdl.render_fill_rect renderer (Some rect_int))
    );
    
    if etat.show_names then begin
      dessiner_texte (cx + 12) (cy - 4) noeud.nom (0,0,0)
    end

(* dessine uniquement le graphe (aretes + noeuds), sans effacer ni synchroniser.
*)
let draw_graph etat =
  Hashtbl.iter (fun _ groupe ->
    let n = List.length groupe in
    List.iteri (fun i a -> draw_arete etat a i n) groupe
  ) etat.aretes_groupees;
  
  (*Dessin des noeuds *)
  List.iter (draw_noeud etat) etat.noeuds_visibles

let rafraichir_ecran etat =
  Sdl_area.clear (get_canvas ());
  draw_graph etat;
  Sdl_area.update (get_canvas ())

