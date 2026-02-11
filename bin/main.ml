open Metrocaml.Network


let chatelet = { id = "chatelet"; name = "Chatelet"; longitude = 2.347; latitude = 48.858; pmr = true }
let bastille = { id = "bastille"; name = "Bastille"; longitude = 2.369; latitude = 48.853; pmr = true }
let gare_lyon = { id = "gare_lyon"; name = "Gare de Lyon"; longitude = 2.373; latitude = 48.844; pmr = true }
let nation = { id = "nation"; name = "Nation"; longitude = 2.396; latitude = 48.848; pmr = true }

let ligne1 = { name = "1"; color = "FFCD00" }
let ligne14 = { name = "14"; color = "6ECA97" }
let rer_a = { name = "A"; color = "E3051C" }

let () =
  let graph = TransportGraph.create () in
  

  let conn1 = { line = ligne1; duration = 2; distance = 0.5; mode = Metro } in
  TransportGraph.add_edge graph chatelet bastille conn1;
  TransportGraph.add_edge graph bastille gare_lyon conn1;

  let conn14 = { line = ligne14; duration = 3; distance = 0.8; mode = Metro } in
  TransportGraph.add_edge graph chatelet gare_lyon conn14;
  let conn_rer = { line = rer_a; duration = 4; distance = 1.2; mode = RER } in
  TransportGraph.add_edge graph chatelet nation conn_rer;
  TransportGraph.add_edge graph nation gare_lyon conn_rer;
  
  Gui.Transport_view.affiche graph
