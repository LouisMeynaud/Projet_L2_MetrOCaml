open Graph_display
open Metrocaml.Network

let color_of_string s =
  let s = String.trim s in
  if String.length s = 0 then 
    0x000000
  else
    let s = if s.[0] = '#' then String.sub s 1 (String.length s - 1) else s in
    try 
      int_of_string ("0x" ^ s)
    with _ -> 
      0x000000

let station_to_noeud s = { id = s.id; nom = s.name; x = s.longitude; y = s.latitude }

let connexion_to_arete src dst conn =
  {
    source = src.id;
    destination = dst.id;
    couleur = color_of_string conn.line.color;
    categorie = string_of_mode conn.mode;
    groupe = conn.line.name;
  }

let aretes_depuis graph source =
  let voisins = TransportGraph.neighbors graph source in
  List.map
    (fun (destination, conn) -> connexion_to_arete source destination conn)
    voisins

let extraire_aretes graph =
  let stations = TransportGraph.nodes graph in
  let aretes_par_station = List.map (aretes_depuis graph) stations in
  List.flatten aretes_par_station

(* ajouter les algo/poids ici pour qu'il aparraise dans l'ui graphique*)
let algos_disponibles = ["Dijkstra"; "BFS"]
let poids_disponibles = ["Temps"; "Distance"; "Arrets"; "Correspondances"; "Combinaison"]

let affiche graph on_search =
  let stations = TransportGraph.nodes graph in
  let noeuds = List.map station_to_noeud stations in
  let aretes = extraire_aretes graph in
  run noeuds aretes algos_disponibles poids_disponibles on_search
