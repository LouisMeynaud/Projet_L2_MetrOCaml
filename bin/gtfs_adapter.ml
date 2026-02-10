(* gtfs_adapter.ml *)

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


let duration_between arrival_str departure_str =
  let diff = time_to_seconds arrival_str - time_to_seconds departure_str in
  if diff < 0 then 0 else diff

let get_index header field =
  match List.find_index (fun h -> String.trim h = field) header with
  | Some i -> i
  | None -> failwith ("Colonne manquante : " ^ field)

let load_network_gtfs directory target_date target_time_seconds =
  let graph = TransportGraph.create () in
  let stations = Hashtbl.create 1000 in
  let cd_csv = Csv.load (Filename.concat directory "calendar_dates.txt") in
  let cd_header = List.hd cd_csv in
  let i_cd_service = get_index cd_header "service_id" in
  let i_cd_date = get_index cd_header "date" in
  let valid_services = Hashtbl.create 100 in
  List.iter (fun row ->
    if List.nth row i_cd_date = target_date then
      Hashtbl.add valid_services (List.nth row i_cd_service) true) (List.tl cd_csv);
  let r_csv = Csv.load (Filename.concat directory "routes.txt") in
  let r_header = List.hd r_csv in
  let i_rid = get_index r_header "route_id" in
  let i_rname = get_index r_header "route_short_name" in
  let i_rtype = get_index r_header "route_type" in
  let route_info = Hashtbl.create 100 in
  List.iter (fun row ->
    let m = match List.nth row i_rtype with
      | "0" -> Tram 
      | "1" -> Metro 
      | "2" -> RER 
      | "3" -> Bus 
      | other -> Custom other in 
      Hashtbl.add route_info (List.nth row i_rid) (List.nth row i_rname, m)) (List.tl r_csv);
  let t_csv = Csv.load (Filename.concat directory "trips.txt") in
  let t_header = List.hd t_csv in
  let i_tid = get_index t_header "trip_id" in
  let i_tsid = get_index t_header "service_id" in
  let i_trid = get_index t_header "route_id" in
  let valid_trips = Hashtbl.create 1000 in
  List.iter (fun row ->
    let sid = List.nth row i_tsid in
    if Hashtbl.mem valid_services sid then
      let info = try Hashtbl.find route_info (List.nth row i_trid) with Not_found -> ("?", Bus) in
      Hashtbl.add valid_trips (List.nth row i_tid) info ) (List.tl t_csv);
  let s_csv = Csv.load (Filename.concat directory "stops.txt") in
  let s_header = List.hd s_csv in
  let i_sid = get_index s_header "stop_id" in
  let i_sname = get_index s_header "stop_name" in
  List.iter (fun row ->
    let id = List.nth row i_sid in
    let station = { Station.id; name = List.nth row i_sname; lines = [] } in
    Hashtbl.add stations id station;
    TransportGraph.add_node graph station) (List.tl s_csv);
  let st_ic = open_in (Filename.concat directory "stop_times.txt") in
  let st_csv = Csv.of_channel st_ic in
  let st_header = Csv.next st_csv in
  let i_st_trip = get_index st_header "trip_id" in
  let i_st_arr = get_index st_header "arrival_time" in
  let i_st_dep = get_index st_header "departure_time" in
  let i_st_id = get_index st_header "stop_id" in
  let last_trip_id = ref "" in
  let last_stop_id = ref "" in
  let last_departure = ref "" in
  let last_is_valid = ref false in
  Csv.iter (fun row ->
    let tid = List.nth row i_st_trip in
    let arr_str = List.nth row i_st_arr in
    let arr_time = time_to_seconds arr_str in
    if Hashtbl.mem valid_trips tid && arr_time >= target_time_seconds then begin
      let stop_id = List.nth row i_st_id in
      let (line_name, m) = Hashtbl.find valid_trips tid in
      (match Hashtbl.find_opt stations stop_id with
       | Some s -> if not (List.mem line_name s.lines) then Hashtbl.replace stations stop_id { s with lines = line_name :: s.lines }
       | _ -> ());
      if tid = !last_trip_id && !last_is_valid then begin
        match (Hashtbl.find_opt stations !last_stop_id, Hashtbl.find_opt stations stop_id) with
        | (Some f, Some t) ->
            let d = arr_time - (time_to_seconds !last_departure) in
            TransportGraph.add_edge graph f t { line = line_name; duration = max d 0; distance = None; mode = m }
        | _ -> ()
      end;
      last_trip_id := tid;
      last_stop_id := stop_id;
      last_departure := List.nth row i_st_dep;
      last_is_valid := true
    end else begin
      last_is_valid := false
    end) st_csv;
  close_in st_ic;
  let tr_file = Filename.concat directory "transfers.txt" in
  if Sys.file_exists tr_file then begin
    let tr_csv = Csv.load tr_file in
    let i_from = get_index (List.hd tr_csv) "from_stop_id" in
    let i_to = get_index (List.hd tr_csv) "to_stop_id" in
    let i_time = get_index (List.hd tr_csv) "min_transfer_time" in
    List.iter (fun row ->
      match (Hashtbl.find_opt stations (List.nth row i_from), Hashtbl.find_opt stations (List.nth row i_to)) with
      | (Some s1, Some s2) when s1.id <> s2.id ->
          let t = try int_of_string (List.nth row i_time) with _ -> 180 in
          TransportGraph.add_edge graph s1 s2 { line = "Transfert"; duration = t; distance = None; mode = Walk }
      | _ -> ()
    ) (List.tl tr_csv)
  end;
  (graph, stations)