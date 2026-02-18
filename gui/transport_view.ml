open Graph_display
open Metrocaml.Network

let color_of_string s =
  let s = if s.[0] = '#' then String.sub s 1 (String.length s - 1) else s in
  int_of_string ("0x" ^ s)


let station_to_noeud s = { id = s.id; nom = s.name; x = s.longitude; y = s.latitude }

let connexion_to_arete src dst conn =
  { source = src.id;
    destination = dst.id;
    couleur = color_of_string conn.line.color;
    categorie = string_of_mode conn.mode;
    groupe = conn.line.name }



let aretes_depuis graph source =
  let voisins = TransportGraph.neighbors graph source in
  List.map (fun (destination, conn) -> connexion_to_arete source destination conn) voisins

let extraire_aretes graph =
  let stations = TransportGraph.nodes graph in
  let aretes_par_station = List.map (aretes_depuis graph) stations in
  List.flatten aretes_par_station


let affiche graph =
  let stations = TransportGraph.nodes graph in
  let noeuds = List.map station_to_noeud stations in
  let aretes = extraire_aretes graph in
  run noeuds aretes