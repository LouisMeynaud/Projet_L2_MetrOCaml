(* gtfs_adapter.ml *)

open Metrocaml 
open Network


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


    let get_day_column date_str =
      try
        let y = int_of_string (String.sub date_str 0 4) in
        let m = int_of_string (String.sub date_str 4 2) in
        let d = int_of_string (String.sub date_str 6 2) in
        let tm = {
          Unix.tm_sec = 0; Unix.tm_min = 0; Unix.tm_hour = 12;
          Unix.tm_mday = d; Unix.tm_mon = m - 1; Unix.tm_year = y - 1900;
          Unix.tm_wday = 0; Unix.tm_yday = 0; Unix.tm_isdst = false
        } in
        let (_, tm_normalized) = Unix.mktime tm in
        match tm_normalized.Unix.tm_wday with
        | 1 -> "monday" | 2 -> "tuesday" | 3 -> "wednesday"
        | 4 -> "thursday" | 5 -> "friday" | 6 -> "saturday" | 0 -> "sunday"
        | _ -> failwith "Erreur jour"
      with _ -> failwith "Format date invalide (YYYYMMDD)"


let duration_between arrival_str departure_str =
  let diff = time_to_seconds arrival_str - time_to_seconds departure_str in
  if diff < 0 then 0 else diff

let get_index header field =
  match List.find_index (fun h -> String.trim h = field) header with
  | Some i -> i
  | None -> failwith ("Colonne manquante : " ^ field)
let get_index_opt header field =
  List.find_index (fun h -> String.trim h = field) header
  let load_network_gtfs directory target_date target_time_seconds =
    Printf.printf "Chargement GTFS depuis %s pour le %s...\n" directory target_date;
    let graph = Network.TransportGraph.create () in
    let valid_services = Hashtbl.create 100 in
    let c_path = Filename.concat directory "calendar.txt" in
    if Sys.file_exists c_path then begin
      let c_csv = Csv.load c_path in
      let c_header = List.hd c_csv in
      let i_service = get_index c_header "service_id" in
      let i_start = get_index c_header "start_date" in
      let i_end = get_index c_header "end_date" in
      let day_col = get_day_column target_date in
      let i_day = get_index c_header day_col in
  
      List.iter (fun row ->
        let start_d = List.nth row i_start in
        let end_d = List.nth row i_end in
        let runs = List.nth row i_day in
        if target_date >= start_d && target_date <= end_d && runs = "1" then
          Hashtbl.add valid_services (List.nth row i_service) true
      ) (List.tl c_csv)
    end else failwith "calendar.txt introuvable";
    let r_csv = Csv.load (Filename.concat directory "routes.txt") in
    let r_header = List.hd r_csv in
    let i_rid = get_index r_header "route_id" in
    let i_rname = get_index r_header "route_short_name" in
    let i_rtype = get_index r_header "route_type" in
    let i_rcolor_opt = get_index_opt r_header "route_color" in
    let route_info = Hashtbl.create 100 in
    
    List.iter (fun row ->
      let mode = match List.nth row i_rtype with
        | "0" -> Network.Tram | "1" -> Network.Metro | "2" -> Network.RER 
        | "3" -> Network.Bus | other -> Network.Custom other in
      
      let color_str = match i_rcolor_opt with
        | Some i -> (try List.nth row i with _ -> "000000")
        | None -> "000000" (* Noir par défaut *)
      in
      let line_obj = { 
        Network.name = List.nth row i_rname; 
        Network.color = color_str 
      } in
      
      Hashtbl.add route_info (List.nth row i_rid) (line_obj, mode)
    ) (List.tl r_csv);
  
    (* C. TRIPS *)
    let t_csv = Csv.load (Filename.concat directory "trips.txt") in
    let t_header = List.hd t_csv in
    let i_tid = get_index t_header "trip_id" in
    let i_tsid = get_index t_header "service_id" in
    let i_trid = get_index t_header "route_id" in
    let valid_trips = Hashtbl.create 1000 in
    let default_line = { Network.name = "?"; Network.color = "000000" } in
  
    List.iter (fun row ->
      let sid = List.nth row i_tsid in
      if Hashtbl.mem valid_services sid then
        let info = try Hashtbl.find route_info (List.nth row i_trid) 
                   with Not_found -> (default_line, Network.Bus) in
        Hashtbl.add valid_trips (List.nth row i_tid) info 
    ) (List.tl t_csv);
    let stations = Hashtbl.create 1000 in
    let s_csv = Csv.load (Filename.concat directory "stops.txt") in
    let s_header = List.hd s_csv in
    let i_sid = get_index s_header "stop_id" in
    let i_sname = get_index s_header "stop_name" in
    let i_lat = get_index s_header "stop_lat" in
    let i_lon = get_index s_header "stop_lon" in
    
    List.iter (fun row ->
      let id = List.nth row i_sid in
      let station = {
        Network.id = id; 
        Network.name = List.nth row i_sname; 
        Network.latitude = float_of_string (List.nth row i_lat);
        Network.longitude = float_of_string (List.nth row i_lon);
        Network.pmr = false 
      } in
      Hashtbl.add stations id station;
      Network.TransportGraph.add_node graph station) (List.tl s_csv);
  
    (* E. STOP TIMES (ARCS) *)
    let st_ic = open_in (Filename.concat directory "stop_times.txt") in
    let st_csv = Csv.of_channel st_ic in
    let st_header = Csv.next st_csv in
    let i_st_trip = get_index st_header "trip_id" in
    let i_st_arr = get_index st_header "arrival_time" in
    let i_st_dep = get_index st_header "departure_time" in
    let i_st_id = get_index st_header "stop_id" in
    
    let last_trip_state = ref None in 
  
    Csv.iter (fun row ->
      let tid = List.nth row i_st_trip in
      let arr_str = List.nth row i_st_arr in
      let arr_time = time_to_seconds arr_str in
      let dep_time = time_to_seconds (List.nth row i_st_dep) in
  
      if Hashtbl.mem valid_trips tid && arr_time >= target_time_seconds then begin
        let stop_id = List.nth row i_st_id in
        let (line_obj, m) = Hashtbl.find valid_trips tid in
        
        match !last_trip_state with
        | Some (prev_tid, prev_stop, prev_dep) when prev_tid = tid ->
            (match (Hashtbl.find_opt stations prev_stop, Hashtbl.find_opt stations stop_id) with
             | (Some f, Some t) ->
                 let d = arr_time - prev_dep in
                 Network.TransportGraph.add_edge graph f t { 
                   Network.line = line_obj; 
                   Network.duration = max d 0; 
                   Network.distance = 0.; 
                   Network.mode = m 
                 }
             | _ -> ());
             last_trip_state := Some (tid, stop_id, dep_time)
        | _ ->
             last_trip_state := Some (tid, stop_id, dep_time)
      end else begin
        last_trip_state := None
      end) st_csv;
    
    close_in st_ic;
    let tr_file = Filename.concat directory "transfers.txt" in
    if Sys.file_exists tr_file then begin
      let tr_csv = Csv.load tr_file in
      let tr_header = List.hd tr_csv in 
      let i_from = get_index tr_header "from_stop_id" in
      let i_to = get_index tr_header "to_stop_id" in
      let i_time = get_index tr_header "min_transfer_time" in
      let transfer_line = { Network.name = "Transfert"; Network.color = "000000" } in
  
      List.iter (fun row ->
        match (Hashtbl.find_opt stations (List.nth row i_from), Hashtbl.find_opt stations (List.nth row i_to)) with
        | (Some s1, Some s2) when s1.id <> s2.id ->
            let t = try int_of_string (List.nth row i_time) with _ -> 180 in
            Network.TransportGraph.add_edge graph s1 s2 { 
              Network.line = transfer_line;
              Network.duration = t; 
              Network.distance = 0.; 
              Network.mode = Network.Walk 
            }
        | _ -> ()
      ) (List.tl tr_csv)
    end;
    
    (graph, stations)