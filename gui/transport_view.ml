open Routing.Graph_display

let station_to_noeud (id, nom, x, y) =
  { id; nom; x; y }
let connexion_to_arete (src, dst, couleur, cat, grp) =
  { source = src; destination = dst; couleur; categorie = cat; groupe = grp }
let affiche stations connexions =
  let noeuds = List.map station_to_noeud stations in
  let aretes = List.map connexion_to_arete connexions in
  run noeuds aretes