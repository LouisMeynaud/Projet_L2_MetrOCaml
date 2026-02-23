(* bin/gtfs_adapter.ml *)
open Network

(* --- 1. Utilitaires --- *)
let time_to_seconds time_str =
  try
  let clean_str = String.trim time_str in
  match String.split_on_char ':' clean_str with
  | [h; m; s] ->
  let hours = int_of_string h in
  let minutes = int_of_string m in
  let seconds = int_of_string s in
  (hours * 3600) + (minutes * 60) + seconds
  | _ -> 0
  with _ -> 0

let get_index header field =
  let field = String.trim field in
  match List.find_index (fun h -> String.trim h = field) header with
  | Some i -> i
  | None -> failwith ("Colonne OBLIGATOIRE manquante : " ^ field)

let get_index_opt header field =
  let field = String.trim field in
  List.find_index (fun h -> String.trim h = field) header

(* --- 2. Chargement --- *)

let load_network_gtfs directory target_date target_time_seconds =
  Printf.printf "Chargement GTFS (Fusion des Areas) depuis %s...\n" directory;
  flush stdout;
  let graph = Network.TransportGraph.create () in
  let valid_services = Hashtbl.create 1000 in
  let load_services name =
    let path = Filename.concat directory name in
    if Sys.file_exists path then
      let csv = Csv.load path in
      let i_sid = get_index (List.hd csv) "service_id" in
      List.iter (fun row -> Hashtbl.replace valid_services (List.nth row i_sid) true) (List.tl csv)
  in
  load_services "calendar.txt";
  load_services "calendar_dates.txt";

  (* B. Routes *)
  let route_info = Hashtbl.create 1000 in
  let r_csv = Csv.load (Filename.concat directory "routes.txt") in
  let r_header = List.hd r_csv in
  let i_rid = get_index r_header "route_id" in
  let i_rname = get_index r_header "route_short_name" in
  let i_rtype = get_index r_header "route_type" in
  let i_rcolor_opt = get_index_opt r_header "route_color" in

  List.iter (fun row ->
    let mode = match List.nth row i_rtype with
      | "0" -> Network.Tram 
      | "1" -> Network.Metro 
      | "2" -> Network.RER 
      | "3" -> Network.Bus 
      | "4" -> Network.Custom "Ferry"
      | "5" -> Network.Custom "Tram à câble"
      | "6" -> Network.Custom "Téléphérique"
      | "7" -> Network.Custom "Funiculaire"
      | "11" -> Network.Custom "TrolleyBus"
      | "12" -> Network.Custom "Monorail"
      | other -> Network.Custom other in
    let color_str = match i_rcolor_opt with
      | Some i -> (try List.nth row i with _ -> "000000")
      | None -> "000000" in
    let line_obj = { Network.name = List.nth row i_rname; Network.color = color_str } in
    Hashtbl.add route_info (List.nth row i_rid) (line_obj, mode)
  ) (List.tl r_csv);

  (* C. Trips *)
  let valid_trips = Hashtbl.create 5000 in
  let t_csv = Csv.load (Filename.concat directory "trips.txt") in
  let t_header = List.hd t_csv in
  let i_tid = get_index t_header "trip_id" in
  let i_tsid = get_index t_header "service_id" in
  let i_trid = get_index t_header "route_id" in

  List.iter (fun row ->
    let sid = List.nth row i_tsid in
    if Hashtbl.mem valid_services sid then
      let info = try Hashtbl.find route_info (List.nth row i_trid) 
                 with Not_found -> ({name="?"; color="000000"}, Bus) in
      Hashtbl.add valid_trips (List.nth row i_tid) info 
  ) (List.tl t_csv);

  (* D. Stops (FUSION DES AREAS) *)
  let stop_to_area = Hashtbl.create 5000 in (* Pour rediriger les sous-stations vers leur Area *)
  let area_nodes = Hashtbl.create 1000 in   (* Pour stocker uniquement les grosses stations *)
  
  let s_csv = Csv.load (Filename.concat directory "stops.txt") in
  let s_header = List.hd s_csv in
  let i_sid = get_index s_header "stop_id" in
  let i_sname = get_index s_header "stop_name" in
  let i_lat = get_index s_header "stop_lat" in
  let i_lon = get_index s_header "stop_lon" in
  let i_parent_opt = get_index_opt s_header "parent_station" in
  
  List.iter (fun row ->
    let raw_id = List.nth row i_sid in
    
    (* On cherche si cet arrêt a une Area parent *)
    let parent_id = match i_parent_opt with
      | Some idx -> 
          let p = try String.trim (List.nth row idx) with _ -> "" in
          if p = "" then raw_id else p (* Si pas de parent, il est son propre parent *)
      | None -> raw_id
    in
    
    (* On dit à l'algo : "Si tu vois raw_id, remplace-le par parent_id" *)
    Hashtbl.replace stop_to_area raw_id parent_id;

    (* On crée la station avec l'ID de l'Area *)
    let station = {
      Network.id = parent_id; 
      name = List.nth row i_sname; 
      latitude = float_of_string (List.nth row i_lat);
      longitude = float_of_string (List.nth row i_lon);
      pmr = false 
    } in

    (* On ne l'ajoute à nos Area que si elle n'y est pas déjà.
       Si c'est le "vrai" parent (raw_id = parent_id), on écrase pour avoir ses vraies coordonnées *)
    if raw_id = parent_id then
      Hashtbl.replace area_nodes parent_id station
    else if not (Hashtbl.mem area_nodes parent_id) then
      Hashtbl.add area_nodes parent_id station

  ) (List.tl s_csv);

  (* On ajoute uniquement les "Areas" (les stations fusionnées) au graphe final *)
  Hashtbl.iter (fun _ st -> Network.TransportGraph.add_node graph st) area_nodes;

  (* E. Stop Times *)
  let st_ic = open_in (Filename.concat directory "stop_times.txt") in
  let st_csv = Csv.of_channel st_ic in
  let st_header = Csv.next st_csv in
  let i_st_trip = get_index st_header "trip_id" in
  let i_st_arr = get_index st_header "arrival_time" in
  let i_st_dep = get_index st_header "departure_time" in
  let i_st_id = get_index st_header "stop_id" in
  
  let last_trip_state = ref None in 
  let existing_edges = Hashtbl.create 10000 in
  
  Csv.iter ~f:(fun row ->
    let tid = List.nth row i_st_trip in
    if Hashtbl.mem valid_trips tid then begin
      let raw_stop_id = List.nth row i_st_id in
      
      (* ---> MAGIE ICI : On remplace le quai par sa grosse Area <--- *)
      let area_id = try Hashtbl.find stop_to_area raw_stop_id with Not_found -> raw_stop_id in

      let arr_time = time_to_seconds (List.nth row i_st_arr) in
      let dep_time = time_to_seconds (List.nth row i_st_dep) in
      let (line_obj, m) = Hashtbl.find valid_trips tid in

      match !last_trip_state with
      | Some (prev_tid, prev_area, prev_dep) when prev_tid = tid ->
          (* On ne crée le trait QUE si le bus se déplace d'une Area à une autre Area différente *)
          if prev_area <> area_id then begin
            match (Hashtbl.find_opt area_nodes prev_area, Hashtbl.find_opt area_nodes area_id) with
            | (Some f, Some t) ->
                let key = (f.id, t.id, line_obj.name) in
                if not (Hashtbl.mem existing_edges key) then begin
                  Network.TransportGraph.add_edge graph f t { 
                    Network.line = line_obj;
                    Network.duration = max (arr_time - prev_dep) 0; 
                    Network.distance = 0.; 
                    Network.mode = m 
                  };
                  Hashtbl.add existing_edges key true
                end
            | _ -> ()
          end;
          last_trip_state := Some (tid, area_id, dep_time)
      | _ ->
          last_trip_state := Some (tid, area_id, dep_time)
    end
  ) st_csv;
  
  close_in st_ic;
  graph