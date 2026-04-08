open Network

let find_station graphe id =
  List.find_opt (fun (s : station) -> s.id = id) (TransportGraph.nodes graphe)

let find_station_by_name graphe name =
  List.find_opt
    (fun (s : station) ->
      String.lowercase_ascii (String.trim s.name)
      = String.lowercase_ascii (String.trim name))
    (TransportGraph.nodes graphe)

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

let is_pmr (s : station) = s.pmr