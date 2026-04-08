open Network
open Lwt.Infix
module Http_client = Cohttp_lwt_unix.Client

(* Type représentant un point dans l'espace (station ou coordonnées géographiques) *)
type endpoint = Station of Network.station | Geo of float * float


(* ----- NOMINATIM ----- *)
(* Fonctions auxiliaires pour récupérer les coordonnées d'une adresse via l'API Nominatim d'OpenStreetMap. *)
(* cf. opam cookbook pour la doc Cohttp et lwt *)

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


(** Géocode une adresse en utilisant l'API Nominatim
  * @param address L'adresse à géocoder
  * @return Un tuple contenant les coordonnées (latitude, longitude) ou None si non trouvées
*)
let geocode_address address =
  let query = String.trim address in
  if query = "" then None else Lwt_main.run (geocode_address_lwt query)

(* --------------------- *)


(** Parse les coordonnées GPS d'une chaîne de caractères
  * @param input La chaîne de caractères contenant les coordonnées
  * @return Un tuple contenant les coordonnées (latitude, longitude) ou None si non valides
*)
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


(** Résout un endpoint en fonction de la requête (id, nom, coordonnées ou adresse)
  * @param graphe Le graphe de transport
  * @param query La requête à résoudre
  * @return L'endpoint résolu ou None si non trouvé
*)
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
            | None -> (
              match geocode_address q with
              | Some (lat, lon) -> Some (Geo (lat, lon))
              | None -> None)))

let walk_speed_m_per_s = 1.0

(** Crée une connexion de liaison à pied entre deux stations
  * @param to_station La station de destination
  * @param distance La distance entre les stations
  * @return La connexion à pied
*)
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

(** Ajoute une station "virtuelle" avec des liens à pied pour représenter le point de départ ou d'arrivée de la recherche courante
  * @param graph Le graphe de transport
  * @param id L'ID de la station à créer
  * @param name Le nom de la station à créer
  * @param lat La latitude de la station à créer
  * @param lon La longitude de la station à créer
  * @param walk_radius Le rayon de marche autour de la station à créer
  * @return La station virtuelle créée et le nombre de stations proches (avec lequelles elle est désormais connectée)
*)
let add_virtual_station_with_walk_edges graph ~id ~name ~lat ~lon ~walk_radius =
  let virtual_station = { Network.id; name; latitude = lat; longitude = lon; pmr = true }
  in
  TransportGraph.add_node graph virtual_station;
  let nearby_count = ref 0 in
  List.iter
    (fun station ->
      let distance =
        Util.haversine_distance lat lon station.latitude station.longitude
      in
      if distance <= walk_radius then (
        incr nearby_count;
        TransportGraph.add_edge graph virtual_station station
          (make_walk_connection ~to_station:station ~distance);
        TransportGraph.add_edge graph station virtual_station
          (make_walk_connection ~to_station:virtual_station ~distance)))
    (TransportGraph.nodes graph);
  (virtual_station, !nearby_count)

let walk_radius = 1000.0

(** Construit le contexte de routage de la recherche courante à partir des requêtes de départ et d'arrivée
  * Lors d'une recherche, on construit des stations "virtuelles" pour représenter le point de départ et d'arrivée,
  * avec des connexions à pied vers les stations réelles proches pour les relier au graphe.
  * @param graph Le graphe de transport
  * @param source_query La requête de départ
  * @param dest_query La requête d'arrivée
  * @return Le contexte de routage ou None si requêtes invalides ou inaccessibles
*)
let build_routing_context graph source_query dest_query =
  match (resolve_endpoint graph source_query, resolve_endpoint graph dest_query) with
  | Some source_ep, Some dest_ep ->
      let routing_graph = Util.clone_graph graph in
      let source, source_links =
        match source_ep with
        | Station station ->
            add_virtual_station_with_walk_edges routing_graph
              ~id:"__source__"
              ~name:("Départ: " ^ station.name)
              ~lat:station.latitude ~lon:station.longitude ~walk_radius
        | Geo (lat, lon) ->
            add_virtual_station_with_walk_edges routing_graph
              ~id:"__source__" ~name:"Point de départ" ~lat ~lon ~walk_radius
      in
      let dest, dest_links =
        match dest_ep with
        | Station station ->
            add_virtual_station_with_walk_edges routing_graph
              ~id:"__destination__"
              ~name:("Arrivée: " ^ station.name)
              ~lat:station.latitude ~lon:station.longitude ~walk_radius
        | Geo (lat, lon) ->
            add_virtual_station_with_walk_edges routing_graph
              ~id:"__destination__" ~name:"Point d'arrivée" ~lat ~lon ~walk_radius
      in
      if source_links = 0 then
        Printf.printf "Aucune station trouvée à moins de %.0fm du point de départ.\n%!" walk_radius;
      if dest_links = 0 then
        Printf.printf "Aucune station trouvée à moins de %.0fm du point d'arrivée.\n%!" walk_radius;
      Some (routing_graph, source, dest)
  | _ -> None
