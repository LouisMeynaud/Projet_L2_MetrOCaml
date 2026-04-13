open Gui_main
open Gui_types
open Metrocaml.Network

(** Convertit une chaîne de caractères en couleur 
  @param s La chaîne de caractères à convertir
  @return La couleur correspondante *)
let color_of_string s =
  let s = String.trim s in (* retire les espaces en début et fin de chaîne *)
  if String.length s = 0 then 0x000000
  else
    let s = if s.[0] = '#' then String.sub s 1 (String.length s - 1) else s in
    try int_of_string ("0x" ^ s) with _ -> 0x000000

(** Convertit une station du graphe de transport en un noeud pour l'affichage
  @param s La station à convertir
  @return Le noeud correspondant *)
let station_to_noeud s =
  { id = s.id; nom = s.name; x = s.longitude; y = s.latitude; pmr = s.pmr }

(** Convertit une connexion en une arête pour l'affichage
  @param src La station de départ
  @param dst La station d'arrivée
  @param conn La connexion à convertir
  @return L'arête correspondante *)
let connexion_to_arete src dst (conn : Metrocaml.Network.connection) =
  {
    source = src.id;
    destination = dst.id;
    couleur = color_of_string conn.line.color;
    categorie = string_of_mode conn.mode;
    groupe = conn.line.name;
  }

(** Extrait les arêtes partant d'une station 
  @param graph Le graphe de transport
  @param source La station de départ
  @return La liste des arêtes partant de la station *)
let aretes_depuis graph source =
  let voisins = TransportGraph.neighbors graph source in
  List.map
    (fun (destination, conn) -> connexion_to_arete source destination conn)
    voisins

(** Extrait toutes les arêtes du graphe 
  @param graph Le graphe de transport
  @return La liste de toutes les arêtes *)
let extraire_aretes graph =
  let stations = TransportGraph.nodes graph in
  let aretes_par_station = List.map (aretes_depuis graph) stations in
  List.flatten aretes_par_station


(** Extrait tous les horaires du graphe 
  @param graph Le graphe de transport
  @return La liste de tous les horaires *)
let extraire_horaires graph =
  let stations = TransportGraph.nodes graph in
    let horaires_par_station =
    List.map
      (fun source ->
        let voisins = TransportGraph.neighbors graph source in
        List.map
          (fun (destination, conn) ->
            {
              mode_name = String.lowercase_ascii (string_of_mode conn.mode);
              line_name = conn.line.name;
              line_color = color_of_string conn.line.color;
              station_id = source.id;
              station_name = source.name;
              direction = conn.terminus.name;
              times = List.map snd conn.schedules;
            })
          voisins)
      stations in
        List.flatten horaires_par_station

let algos_disponibles = [ "Dijkstra"; "BFS" ]

let poids_disponibles =
  [
    "Default";
    "Combinaison";
  ]

(** Affiche la fenêtre de l'application avec les données du graphe de transport
  @param graph Le graphe de transport à afficher
  @param on_search La fonction de rappel à appeler lors de la recherche d'itinéraire *)
let affiche graph on_search =
  let noeuds = List.map station_to_noeud (TransportGraph.nodes graph) in
  let aretes = extraire_aretes graph in
  let horaires = extraire_horaires graph in
  run noeuds aretes algos_disponibles poids_disponibles horaires on_search
