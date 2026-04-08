open Network
open Lwt.Infix
module Http_client = Cohttp_lwt_unix.Client

type endpoint = Station of Network.station | Geo of float * float

let json_number_to_float = function
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | `String s -> ( try Some (float_of_string s) with _ -> None)
  | _ -> None

let parse_nominatim_coordinates payload =
  match Yojson.Safe.from_string payload with
  | `List (`Assoc fields :: _) -> (
      let extract field_name =
        match List.assoc_opt field_name fields with
        | Some value -> json_number_to_float value
        | None -> None
      in
      match (extract "lat", extract "lon") with
      | Some lat, Some lon -> Some (lat, lon)
      | _ -> None)
  | _ -> None

(* cf. opam cookbook pour la doc Cohttp et lwt *)
let geocode_address_lwt address =
  let query = String.trim address in
  if query = "" then Lwt.return_none
  else
    let encoded_query = Uri.pct_encode ~component:`Query_value query in
    let uri =
      Uri.of_string
        (Printf.sprintf
           "https://nominatim.openstreetmap.org/search?q=%s&format=jsonv2&limit=1"
           encoded_query)
    in
    let headers =
      Cohttp.Header.of_list
        [ ("User-Agent", "MetroCaml"); ("Accept", "application/json") ]
    in
    Lwt.catch
      (fun () ->
        Lwt_unix.with_timeout 6.0 (fun () ->
            Lwt.bind (Http_client.get ~headers uri) (fun (resp, body) ->
                Printf.printf "Requête Nominatim pour '%s' : %s\n%!" address
                  (Cohttp.Code.string_of_status (Cohttp.Response.status resp));
                let code =
                  resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status
                in
                if Cohttp.Code.is_success code then
                  Lwt.bind (Cohttp_lwt.Body.to_string body) (fun payload ->
                      Lwt.return
                        (try parse_nominatim_coordinates payload
                         with _ -> None))
                else (
                  Printf.eprintf "Erreur HTTP pour '%s': %d %s\n%!" address code
                    (Cohttp.Code.reason_phrase_of_code code);
                  Lwt.return_none))))
      (fun exn ->
        Printf.eprintf "Échec Cohttp pour '%s': %s\n%!" address
          (Printexc.to_string exn);
        Lwt.return_none)

let geocode_address address =
  let query = String.trim address in
  if query = "" then None else Lwt_main.run (geocode_address_lwt query)

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
        | None -> (
            match parse_coordinates q with
            | Some (lat, lon) -> Some (Geo (lat, lon))
            | None ->
                geocode_address q
                |> Option.map (fun (lat, lon) -> Geo (lat, lon))))

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
    { Network.id; name; latitude = lat; longitude = lon; pmr = true }
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
              ~id:"__source__"
              ~name:("Départ: " ^ station.name)
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
