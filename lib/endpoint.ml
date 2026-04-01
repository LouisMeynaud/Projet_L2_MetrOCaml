open Network

type endpoint = Station of Network.station | Geo of float * float

let parse_coordinates input =
  let cleaned =
    String.trim input |> String.map (fun c -> if c = ';' then ',' else c)
  in
  let parse_parts parts =
    match parts with
    | [ lat_s; lon_s ] -> (
        try
          Some
            ( float_of_string (String.trim lat_s),
              float_of_string (String.trim lon_s) )
        with _ -> None)
    | _ -> None
  in
  match
    cleaned |> String.split_on_char ','
    |> List.filter (fun s -> String.trim s <> "")
    |> parse_parts
  with
  | Some coords -> Some coords
  | None ->
      cleaned |> String.split_on_char ' '
      |> List.filter (fun s -> String.trim s <> "")
      |> parse_parts

let resolve_endpoint graphe query =
  let q = String.trim query in
  if q = "" then None
  else
    match Util.find_station graphe q with
    | Some station -> Some (Station station)
    | None -> (
        match Util.find_station_by_name graphe q with
        | Some station -> Some (Station station)
        | None ->
            parse_coordinates q |> Option.map (fun (lat, lon) -> Geo (lat, lon))
        )

let walk_speed_m_per_s = 1.4

let make_walk_connection ~to_station ~distance =
  {
    Network.line = { name = "Marche"; color = "AAAAAA" };
    Network.duration =
      max 1 (int_of_float (ceil (distance /. walk_speed_m_per_s)));
    Network.schedules = [];
    Network.distance;
    Network.mode = Network.Walk;
    Network.terminus = to_station;
  }

let add_virtual_station_with_walk_edges graph (stations : Network.station list)
    ~id ~name ~lat ~lon ~walk_radius =
  let virtual_station =
    { Network.id; name; latitude = lat; longitude = lon; pmr = false }
  in
  TransportGraph.add_node graph virtual_station;
  let nearby_count = ref 0 in
  List.iter
    (fun (station : Network.station) ->
      let distance =
        Util.haversine_distance lat lon station.latitude station.longitude
      in
      if distance <= walk_radius then (
        incr nearby_count;
        TransportGraph.add_edge graph virtual_station station
          (make_walk_connection ~to_station:station ~distance);
        TransportGraph.add_edge graph station virtual_station
          (make_walk_connection ~to_station:virtual_station ~distance)))
    stations;
  (virtual_station, !nearby_count)

let walk_radius = 1000.0

let build_routing_context graph source_query dest_query =
  match
    (resolve_endpoint graph source_query, resolve_endpoint graph dest_query)
  with
  | Some source_ep, Some dest_ep ->
      let routing_graph = Util.clone_graph graph in
      let base_stations = Network.TransportGraph.nodes graph in
      let source, source_links =
        match source_ep with
        | Station station ->
            add_virtual_station_with_walk_edges routing_graph base_stations
              ~id:"__source__" ~name:("Départ: " ^ station.name)
              ~lat:station.latitude ~lon:station.longitude ~walk_radius
        | Geo (lat, lon) ->
            add_virtual_station_with_walk_edges routing_graph base_stations
              ~id:"__source__" ~name:"Point de départ" ~lat ~lon ~walk_radius
      in
      let dest, dest_links =
        match dest_ep with
        | Station station ->
            add_virtual_station_with_walk_edges routing_graph base_stations
              ~id:"__destination__"
              ~name:("Arrivée: " ^ station.name)
              ~lat:station.latitude ~lon:station.longitude ~walk_radius
        | Geo (lat, lon) ->
            add_virtual_station_with_walk_edges routing_graph base_stations
              ~id:"__destination__" ~name:"Point d'arrivée" ~lat ~lon
              ~walk_radius
      in
      if source_links = 0 then
        Printf.printf
          "Aucune station trouvée à moins de %.0fm du point de départ.\n%!"
          walk_radius;
      if dest_links = 0 then
        Printf.printf
          "Aucune station trouvée à moins de %.0fm du point d'arrivée.\n%!"
          walk_radius;
      Some (routing_graph, source, dest)
  | _ -> None
