open Network

(** Recherche une station par son ID
  * @param graphe Le graphe de transport
  * @param id L'ID de la station
  * @return La station correspondante, ou None si non trouvée
  *)
let find_station graphe id =
  List.find_opt (fun (s : station) -> s.id = id) (TransportGraph.nodes graphe)

(** Recherche une station par son nom
  * @param graphe Le graphe de transport
  * @param name Le nom de la station
  * @return La station correspondante, ou None si non trouvée
  *)
let find_station_by_name graphe name =
  List.find_opt
    (fun (s : station) ->
      String.lowercase_ascii (String.trim s.name)
      = String.lowercase_ascii (String.trim name))
    (TransportGraph.nodes graphe)

(** Clone un graphe de transport 
  * @param graph Le graphe à cloner
  * @return Un nouveau graphe identique au graphe d'origine, mais indépendant.
*)
let clone_graph graph =
  let clone = TransportGraph.create () in
  let nodes = TransportGraph.nodes graph in
  List.iter (TransportGraph.add_node clone) nodes;
  List.iter
    (fun source ->
      TransportGraph.neighbors graph source
      |> List.iter (fun (destination, conn) ->
          TransportGraph.add_edge clone source destination conn))
    nodes;
  clone

let to_radians deg = deg *. (4.0 *. atan 1.0) /. 180.0

(** Calcule la distance entre deux couples de coordonnées en utilisant la formule de Haversine
  * @param lat1 Latitude du point 1
  * @param lon1 Longitude du point 1
  * @param lat2 Latitude du point 2
  * @param lon2 Longitude du point 2
  * @return La distance en mètres entre les deux points
*)
let haversine_distance lat1 lon1 lat2 lon2 =
  let r = 6371000.0 in
  let phi1 = to_radians lat1 in
  let phi2 = to_radians lat2 in
  let delta_phi = to_radians (lat2 -. lat1) in
  let delta_lambda = to_radians (lon2 -. lon1) in
  let a =
    (sin (delta_phi /. 2.0) ** 2.0)
    +. (cos phi1 *. cos phi2 *. (sin (delta_lambda /. 2.0) ** 2.0))
  in
  let c = 2.0 *. atan2 (sqrt a) (sqrt (1.0 -. a)) in
  r *. c

(** Vérifie si une station est accessible aux personnes à mobilité réduite *)
let is_pmr (s : station) = s.pmr
