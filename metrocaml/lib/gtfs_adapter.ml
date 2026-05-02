open Network

(* ================================================== *)
(* 1. UTILITAIRES                                     *)
(* ================================================== *)

(** [normalize_name s] nettoie une chaîne de caractères en la passant en minuscules 
    et en conservant uniquement les lettres et les chiffres.
    Essentiel pour faire correspondre des noms de stations saisis par l'utilisateur 
    avec ceux du GTFS, en ignorant la casse, les espaces et les tirets.
    
    @param s La chaîne de caractères brute.
    @return La chaîne normalisée (ex: "Châtelet - Les Halles" -> "chteletleshalles"). *)
let normalize_name s =
  let s = String.lowercase_ascii s in
  let buffer = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | '0'..'9' -> Buffer.add_char buffer c
    | _ -> ()
  ) s;
  Buffer.contents buffer
(** [time_to_seconds time_str] convertit une heure GTFS au format "HH:MM:SS" 
    en un nombre total de secondes écoulées depuis minuit.
    NB: Le standard GTFS autorise des heures > 24h.
    
    @param time_str L'heure sous forme de texte.
    @return L'équivalent en secondes (entier). Renvoie 0 en cas d'erreur de format. *)
let time_to_seconds time_str =
  try
    let clean_str = String.trim time_str in
    match String.split_on_char ':' clean_str with
    | [ h; m; s ] ->
        let hours = int_of_string h in
        let minutes = int_of_string m in
        let seconds = int_of_string s in
        (hours * 3600) + (minutes * 60) + seconds
    | _ -> 0
  with _ -> 0

(** [clean_header_col s] nettoie le nom d'une colonne CSV.
    Permet de retirer les espaces et les  caractères invisibles (\xEF\xBB\xBF) *)
let clean_header_col s =
  let s = String.trim s in
  if String.length s >= 3 && String.sub s 0 3 = "\xEF\xBB\xBF" then
    String.trim (String.sub s 3 (String.length s - 3))
  else s

(** [get_index header field] trouve l'indice d'une colonne [field] dans la ligne d'en-tête [header].
    
    @param header La liste des noms de colonnes.
    @param field Le nom exact de la colonne recherchée.
    @raise Failure si la colonne est introuvable (utilisé pour les champs GTFS obligatoires). *)
let get_index header field =
  let field = String.trim field in
  match
    List.find_index (fun h -> String.trim (clean_header_col h) = field) header
  with
  | Some i -> i
  | None -> failwith ("Colonne OBLIGATOIRE manquante : " ^ field)


(** [get_index_opt header field] trouve l'indice d'une colonne optionnelle [field] dans [header].
    
    @return [Some index] si la colonne existe, [None] sinon. *)
let get_index_opt header field =
  let field = String.trim field in
  List.find_index (fun h -> String.trim (clean_header_col h) = field) header

(** [day_of_week date_str] calcule le jour de la semaine à partir d'une date "YYYYMMDD".
    Utilise  la congruence de Zeller.
    
    @param date_str La date au format texte.
    @return Un entier représentant le jour (0 = dimanche, 1 = lundi, ..., 6 = samedi). *)
let day_of_week date_str =
  try
    let year = int_of_string (String.sub date_str 0 4) in
    let month = int_of_string (String.sub date_str 4 2) in
    let day = int_of_string (String.sub date_str 6 2) in
    let month_offsets = [| 0; 3; 2; 5; 0; 3; 5; 1; 4; 6; 2; 4 |] in
    let adjusted_year = if month < 3 then year - 1 else year in
    let sum =
      adjusted_year + (adjusted_year / 4) - (adjusted_year / 100)
      + (adjusted_year / 400)
      + month_offsets.(month - 1)
      + day
    in
    sum mod 7
  with _ -> 0

(** [next_day_str yyyymmdd] calcule la date du lendemain pour anticiper les trajets nocturnes.
    Prend en charge les changements de mois, d'années, et les années bissextiles.
    
    @param yyyymmdd La date actuelle au format "YYYYMMDD".
    @return La date du lendemain au même format. *)
let next_day_str yyyymmdd =
  try
    let y = int_of_string (String.sub yyyymmdd 0 4) in
    let m = int_of_string (String.sub yyyymmdd 4 2) in
    let d = int_of_string (String.sub yyyymmdd 6 2) in

    let is_leap = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0) in
    let days_in_month =
      [|
        31; (if is_leap then 29 else 28); 31; 30; 31; 30; 31; 31; 30; 31; 30; 31;
      |]
    in

    if d + 1 <= days_in_month.(m - 1) then
      Printf.sprintf "%04d%02d%02d" y m (d + 1)
    else if m + 1 <= 12 then Printf.sprintf "%04d%02d01" y (m + 1)
    else Printf.sprintf "%04d0101" (y + 1)
  with _ -> yyyymmdd

(* ================================================== *)
(* 2. CHARGEMENT DU GRAPHE            *)
(* ================================================== *)

(** [load_network_gtfs directory target_date] est le moteur de construction principal du graphe.
    Il parcourt tous les fichiers normés GTFS d'un répertoire pour extraire le réseau 
    de transport valable à une date précise (et sur les 48h suivantes pour gérer les vo-yages nécessitant plusieurs jours).
      
    @param directory Le chemin du dossier contenant les fichiers .txt du GTFS.
    @param target_date La date ciblée au format "YYYYMMDD".
    @return Un [Network.TransportGraph.t].*)
let load_network_gtfs directory target_date =
  Logger.black "Chargement GTFS (Fusion des Areas) depuis %s...\n%!" directory;
  flush stdout;
  let graph = Network.TransportGraph.create () in
  let valid_services = Hashtbl.create 50000 in
  let date_0 = target_date in
  let date_1 = next_day_str date_0 in
  let date_2 = next_day_str date_1 in

  let day_cols =
    [|
      "sunday";
      "monday";
      "tuesday";
      "wednesday";
      "thursday";
      "friday";
      "saturday";
    |]
  in
  let col_0 = day_cols.(day_of_week date_0) in
  let col_1 = day_cols.(day_of_week date_1) in
  let col_2 = day_cols.(day_of_week date_2) in
  let cal_path = Filename.concat directory "calendar.txt" in
  if Sys.file_exists cal_path then begin
    let csv = Csv.load cal_path in
    if List.length csv > 1 then begin
      let header = List.hd csv in
      let i_sid = get_index header "service_id" in
      let i_start = get_index header "start_date" in
      let i_end = get_index header "end_date" in
      let i_d0 = get_index header col_0 in
      let i_d1 = get_index header col_1 in
      let i_d2 = get_index header col_2 in
      List.iter
        (fun row ->
          let sid = List.nth row i_sid in
          let start_d = String.trim (List.nth row i_start) in
          let end_d = String.trim (List.nth row i_end) in
          let mut_offsets = ref [] in
          if
            date_0 >= start_d && date_0 <= end_d
            && String.trim (List.nth row i_d0) = "1"
          then mut_offsets := 0 :: !mut_offsets;
          if
            date_1 >= start_d && date_1 <= end_d
            && String.trim (List.nth row i_d1) = "1"
          then mut_offsets := 86400 :: !mut_offsets;
          if
            date_2 >= start_d && date_2 <= end_d
            && String.trim (List.nth row i_d2) = "1"
          then mut_offsets := 172800 :: !mut_offsets;
          if !mut_offsets <> [] then
            Hashtbl.replace valid_services sid (List.rev !mut_offsets))
        (List.tl csv)
    end
  end;
  let cal_dates_path = Filename.concat directory "calendar_dates.txt" in
  if Sys.file_exists cal_dates_path then begin
    let csv = Csv.load cal_dates_path in
    if List.length csv > 1 then begin
      let header = List.hd csv in
      let i_sid = get_index header "service_id" in
      let i_date = get_index header "date" in
      let i_exc = get_index header "exception_type" in
      List.iter
        (fun row ->
          let d = String.trim (List.nth row i_date) in
          let sid = List.nth row i_sid in
          let exc = String.trim (List.nth row i_exc) in
          let offset_opt =
            if d = date_0 then Some 0
            else if d = date_1 then Some 86400
            else if d = date_2 then Some 172800
            else None
          in
          match offset_opt with
          | Some offset ->
              let current_offsets =
                try Hashtbl.find valid_services sid with Not_found -> []
              in
              if exc = "1" then begin
                if not (List.mem offset current_offsets) then
                  Hashtbl.replace valid_services sid
                    (List.sort compare (offset :: current_offsets))
              end
              else if exc = "2" then begin
                let filtered = List.filter (fun o -> o <> offset) current_offsets in
                if filtered = [] then Hashtbl.remove valid_services sid
                else Hashtbl.replace valid_services sid filtered
              end
          | None -> ())
        (List.tl csv)
    end
  end;

  (* B. Routes *)
  let route_info = Hashtbl.create 50000 in
  let r_csv = Csv.load (Filename.concat directory "routes.txt") in
  let r_header = List.hd r_csv in
  let i_rid = get_index r_header "route_id" in
  let i_rname = get_index r_header "route_short_name" in
  let i_rtype = get_index r_header "route_type" in
  let i_rcolor_opt = get_index_opt r_header "route_color" in

  List.iter
    (fun row ->
      let mode =
        match List.nth row i_rtype with
        | "0" | "900" -> Network.Tram
        | "1" | "400" -> Network.Metro
        | "2" | "100" -> Network.RER
        | "3" | "700" | "701" | "702" | "704" -> Network.Bus
        | "4" -> Network.Custom "Ferry"
        | "5" -> Network.Custom "Tram à câble"
        | "6" -> Network.Custom "Téléphérique"
        | "7" -> Network.Custom "Funiculaire"
        | "11" -> Network.Custom "TrolleyBus"
        | "12" -> Network.Custom "Monorail"
        | other -> Network.Custom other
      in
      let color_str =
        match i_rcolor_opt with
        | Some i -> ( try List.nth row i with _ -> "000000")
        | None -> "000000"
      in
      let raw_id = List.nth row i_rid in
      let parts = String.split_on_char ':' raw_id in
      let rec find_num = function | [] -> raw_id | h :: t -> if String.length h > 0 && h.[0] >= '0' && h.[0] <= '9' then h else find_num t in
      let clean_id = find_num parts in

      (* 3. On crée l'objet complet *)
      let line_obj =
        { Network.id = clean_id; Network.name = List.nth row i_rname; Network.color = color_str }
    
      in
      Hashtbl.add route_info (List.nth row i_rid) (line_obj, mode))
    (List.tl r_csv);

  (* C. Trips *)
  let valid_trips = Hashtbl.create 200000 in
  let trip_to_block = Hashtbl.create 5000 in
  let t_path = Filename.concat directory "trips.txt" in
  if Sys.file_exists t_path then begin
    let t_ic = open_in t_path in
    let t_csv = Csv.of_channel t_ic in
    let t_header = Csv.next t_csv in
    let i_tid = get_index t_header "trip_id" in
    let i_tsid = get_index t_header "service_id" in
    let i_trid = get_index t_header "route_id" in
    let i_block_opt = get_index_opt t_header "block_id" in
    let i_headsign_opt = get_index_opt t_header "trip_headsign" in

    Csv.iter
      ~f:(fun row ->
        let sid = try List.nth row i_tsid with _ -> "" in
        if Hashtbl.mem valid_services sid then
          let line_obj, mode =
            try Hashtbl.find route_info (List.nth row i_trid)
            with Not_found ->
              ({ Network.name = "?"; color = "000000"; Network.id = "" }, Network.Bus)
          in
          let tid = List.nth row i_tid in
          let headsign = match i_headsign_opt with
            | Some idx -> (try String.trim (List.nth row idx) with _ -> "")
            | None -> ""
          in
          Hashtbl.add valid_trips tid (line_obj, mode, sid, headsign);
          let block = match i_block_opt with
            | Some idx -> (try String.trim (List.nth row idx) with _ -> "")
            | None -> ""
          in
          let vehicle_id = if block = "" then tid else block in
          Hashtbl.add trip_to_block tid vehicle_id)
      t_csv;
    close_in t_ic
  end;

  (* D. Stops *)
  let stop_to_area = Hashtbl.create 15000 in
  let area_nodes = Hashtbl.create 20000 in
  let name_to_station = Hashtbl.create 20000 in
  let s_path = Filename.concat directory "stops.txt" in
  if Sys.file_exists s_path then begin
    let s_ic = open_in s_path in
    let s_csv = Csv.of_channel s_ic in
    let s_header = Csv.next s_csv in
    let i_sid = get_index s_header "stop_id" in
    let i_sname = get_index s_header "stop_name" in
    let i_lat = get_index s_header "stop_lat" in
    let i_lon = get_index s_header "stop_lon" in
    let i_parent_opt = get_index_opt s_header "parent_station" in
    let i_wheelchair_opt = get_index_opt s_header "wheelchair_boarding" in

    Csv.iter
      ~f:(fun row ->
        try
          let raw_id = List.nth row i_sid in
          let parent_id =
            match i_parent_opt with
            | Some idx ->
                let p = try String.trim (List.nth row idx) with _ -> "" in
                if p = "" then raw_id else p
            | None -> raw_id
          in
          Hashtbl.replace stop_to_area raw_id parent_id;
          let station_name = List.nth row i_sname in
          let is_pmr =
            match i_wheelchair_opt with
            | Some idx -> (try String.trim (List.nth row idx) = "1" with _ -> false)
            | None -> false
          in
          let station =
            {
              Network.id = parent_id;
              name = List.nth row i_sname;
              latitude = float_of_string (List.nth row i_lat);
              longitude = float_of_string (List.nth row i_lon);
              pmr = is_pmr;
            }
          in
          let norm_name = normalize_name station_name in
          if not (Hashtbl.mem name_to_station norm_name) then
            Hashtbl.add name_to_station norm_name station;
          if raw_id = parent_id then begin
            let final_pmr =
              match Hashtbl.find_opt area_nodes parent_id with
              | Some existing_node -> existing_node.pmr || is_pmr
              | None -> is_pmr
            in
            Hashtbl.replace area_nodes parent_id { station with pmr = final_pmr }
          end
          else if not (Hashtbl.mem area_nodes parent_id) then
            Hashtbl.add area_nodes parent_id station
          else if is_pmr then begin
            let existing_parent = Hashtbl.find area_nodes parent_id in
            Hashtbl.replace area_nodes parent_id { existing_parent with pmr = true }
          end
        with _ -> ())
      s_csv;
    close_in s_ic
  end;
  Hashtbl.iter (fun _ st -> Network.TransportGraph.add_node graph st) area_nodes;

  (* E+F. Terminus + Stop Times *)
  let vehicle_terminus  = Hashtbl.create 5000 in
  let stop_time_entries = ref [] in
  let st_path = Filename.concat directory "stop_times.txt" in
  if Sys.file_exists st_path then begin
    let st_ic = open_in st_path in
    let st_csv = Csv.of_channel st_ic in
    let st_header = Csv.next st_csv in
    let i_st_trip     = get_index     st_header "trip_id" in
    let i_st_arr      = get_index     st_header "arrival_time" in
    let i_st_dep      = get_index     st_header "departure_time" in
    let i_st_id       = get_index     st_header "stop_id" in
    let i_st_dist_opt = get_index_opt st_header "shape_dist_traveled" in
    Csv.iter ~f:(fun row ->
      let tid = try List.nth row i_st_trip with _ -> "" in
      if Hashtbl.mem valid_trips tid then begin
        let raw_stop_id = try List.nth row i_st_id  with _ -> "" in
        let area_id     = try Hashtbl.find stop_to_area raw_stop_id with Not_found -> raw_stop_id in
        let arr_time    = time_to_seconds (try List.nth row i_st_arr with _ -> "") in
        let dep_time    = time_to_seconds (try List.nth row i_st_dep with _ -> "") in
        let dist_opt    = match i_st_dist_opt with
          | Some idx -> (try Some (float_of_string (List.nth row idx)) with _ -> None)
          | None -> None
        in
        let vehicle_id  = try Hashtbl.find trip_to_block tid with Not_found -> tid in
        let current_max = try fst (Hashtbl.find vehicle_terminus vehicle_id) with Not_found -> -1 in
        if arr_time > current_max then
          Hashtbl.replace vehicle_terminus vehicle_id (arr_time, area_id);
        stop_time_entries := (tid, area_id, arr_time, dep_time, dist_opt) :: !stop_time_entries
      end) st_csv;
    close_in st_ic
  end;

  (* Construction des arêtes *)
  let edges_acc = Hashtbl.create 10000 in
  let last_trip_state = ref None in
  List.iter (fun (tid, area_id, arr_time, dep_time, current_dist_opt) ->
    let line_obj, m, sid, headsign = Hashtbl.find valid_trips tid in
    let vehicle_id = try Hashtbl.find trip_to_block tid with Not_found -> tid in
    let term_area_id = try snd (Hashtbl.find vehicle_terminus vehicle_id) with Not_found -> area_id in
    let physical_term_station = try Hashtbl.find area_nodes term_area_id with Not_found ->
      { Network.id = term_area_id; name = "Inconnu"; latitude = 0.; longitude = 0.; pmr = false }
    in
    let norm_headsign = normalize_name headsign in
    let term_station =
      if norm_headsign <> "" && Hashtbl.mem name_to_station norm_headsign then
        Hashtbl.find name_to_station norm_headsign
      else
        physical_term_station
    in
    (match !last_trip_state with
    | Some (prev_tid, prev_area, prev_dep, prev_dist_opt) when prev_tid = tid ->
        if prev_area <> area_id then begin
          match Hashtbl.find_opt area_nodes prev_area, Hashtbl.find_opt area_nodes area_id with
          | Some f, Some t ->
              let key = (f.id, t.id, line_obj.name, term_station) in
              let duration = max (arr_time - prev_dep) 0 in
              let edge_gtfs_dist = match (prev_dist_opt, current_dist_opt) with
                | Some d1, Some d2 when d2 > d1 -> Some (d2 -. d1)
                | _ -> None
              in
              let active_offsets =
                try Hashtbl.find valid_services sid
                with Not_found -> [ 0 ]
              in
              (match Hashtbl.find_opt edges_acc key with
              | Some (_, _, _, _, _, schedules_ref, dist_ref, _) ->
                  List.iter
                    (fun offset ->
                      schedules_ref := (tid, prev_dep + offset) :: !schedules_ref)
                      active_offsets;
                  if !dist_ref = None then dist_ref := edge_gtfs_dist
              | None ->
                  let initial_schedules =
                    List.map
                      (fun offset -> (tid, prev_dep + offset))
                      active_offsets
                  in
                  Hashtbl.add edges_acc key
                    (f, t, line_obj, duration, m, ref initial_schedules, ref edge_gtfs_dist, term_station))
          | _ -> ()
        end;
        last_trip_state := Some (tid, area_id, dep_time, current_dist_opt)
    | _ -> last_trip_state := Some (tid, area_id, dep_time, current_dist_opt));
  ) (List.rev !stop_time_entries);

  (* G. Construction finale des arêtes avec listes triées *)
  Hashtbl.iter
    (fun _ (f, t, line_obj, duration, m, schedules_ref, dist_ref, term_station) ->
      let sorted_schedules = List.sort (fun (_, temps1) (_, temps2) -> compare temps1 temps2) !schedules_ref in
      let dist_meters =
        match !dist_ref with
        | Some d -> d
        | None ->
            Util.haversine_distance f.latitude f.longitude t.latitude t.longitude
      in
      let conn =
        {
          Network.line = line_obj;
          Network.duration;
          Network.schedules = sorted_schedules;
          Network.distance = dist_meters;
          Network.mode = m;
          Network.terminus = term_station;
        }
      in
      Network.TransportGraph.add_edge graph f t conn)
    edges_acc;
    
  (* H. Transfers (Correspondances à pied) *)
  let tr_path = Filename.concat directory "transfers.txt" in
  if Sys.file_exists tr_path then begin
    let tr_ic = open_in tr_path in
    let tr_csv = Csv.of_channel tr_ic in
    let tr_header = Csv.next tr_csv in
    
    let i_from = get_index tr_header "from_stop_id" in
    let i_to = get_index tr_header "to_stop_id" in
    let i_type = get_index tr_header "transfer_type" in
    let i_time_opt = get_index_opt tr_header "min_transfer_time" in

    Csv.iter ~f:(fun row ->
      try
        let raw_from = List.nth row i_from in
        let raw_to = List.nth row i_to in
        let t_type = String.trim (List.nth row i_type) in
        let area_from = try Hashtbl.find stop_to_area raw_from with Not_found -> raw_from in
        let area_to = try Hashtbl.find stop_to_area raw_to with Not_found -> raw_to in
        if area_from <> area_to && (t_type = "0" || t_type = "2") then begin
          match Hashtbl.find_opt area_nodes area_from, Hashtbl.find_opt area_nodes area_to with
          | Some f, Some t ->
              let walk_dist = Util.haversine_distance f.latitude f.longitude t.latitude t.longitude *. Endpoint.walk_detour_factor in
              let hyp_walk_time = walk_dist /. Endpoint.walk_speed_m_per_s |> int_of_float in

              let walk_time = match i_time_opt with
                | Some idx -> (try int_of_string (List.nth row idx) with _ -> hyp_walk_time)
                | None -> hyp_walk_time
              in
              
              let walk_conn = {
                Network.line = { name = "Marche"; color = "AAAAAA"; id = "" };
                Network.duration = walk_time;
                Network.schedules = [];
                Network.distance = walk_dist;
                Network.mode = Network.Transfert;
                Network.terminus = t;
              } in
              Network.TransportGraph.add_edge graph f t walk_conn;
              Network.TransportGraph.add_edge graph t f { walk_conn with Network.terminus = f }
          | _ -> ()
        end
      with _ -> ()
    ) tr_csv;
    close_in tr_ic
  end; 
  graph

(* ================================================== *)
(* 3. TÉLÉCHARGEMENT ET SYNCHRONISATION               *)
(* ================================================== *)

(** Exception levée lorsque l'URL cible (pour le GTFS) est injoignable, 
    incorrecte, ou renvoie une erreur (ex: Erreur404). *)
exception Invalid_GTFS_URL of string

(** [download_and_extract_gtfs url dest_dir] télécharge un flux GTFS distant 
    compressé (.zip) et l'extrait localement. 
    Utilisable sur MacOs,Windows et Linux.
    
    @param url L'URL de téléchargement direct du fichier ZIP.
    @param dest_dir Le répertoire cible où seront extraits les fichiers CSV.
    @raise Invalid_GTFS_URL si le téléchargement échoue. *)
let download_and_extract_gtfs url dest_dir =
  Logger.black "\n=== Récupération du GTFS ===\n";
  Logger.black "URL : %s\n" url;

  let is_win = Sys.os_type = "Win32" || Sys.os_type = "Cygwin" in
  let zip_path = Filename.concat dest_dir "gtfs_data.zip" in

  let cmd_mkdir =
    if is_win then
      Printf.sprintf "if not exist \"%s\" mkdir \"%s\"" dest_dir dest_dir
    else Printf.sprintf "mkdir -p \"%s\"" dest_dir
  in
  let _ = Sys.command cmd_mkdir in
  Logger.black "Téléchargement en cours...\n";
  flush stdout;
  let cmd_curl =
    if is_win then
      Printf.sprintf "curl.exe -f -L -s -o \"%s\" \"%s\"" zip_path url
    else Printf.sprintf "curl -f -L -s -o \"%s\" \"%s\"" zip_path url
  in

  if Sys.command cmd_curl <> 0 then begin
    let _ =
      Sys.command
        (if is_win then Printf.sprintf "rmdir /S /Q \"%s\"" dest_dir
         else Printf.sprintf "rm -rf \"%s\"" dest_dir)
    in
    raise
      (Invalid_GTFS_URL "Lien introuvable ou erreur de serveur (Code 404/500)")
  end;

  Logger.black "Extraction des fichiers...\n";
  flush stdout;
  let cmd_unzip =
    if is_win then
      Printf.sprintf
        "powershell -noprofile -command \"Expand-Archive -Path '%s' \
         -DestinationPath '%s' -Force\""
        zip_path dest_dir
    else Printf.sprintf "unzip -q -o \"%s\" -d \"%s\"" zip_path dest_dir
  in

  if Sys.command cmd_unzip <> 0 then
    failwith "Erreur : Le fichier telecharge n'est pas un ZIP valide.";

  let cmd_rm =
    if is_win then Printf.sprintf "del /Q \"%s\"" zip_path
    else Printf.sprintf "rm -f \"%s\"" zip_path
  in
  let _ = Sys.command cmd_rm in

  Logger.black "Données prêtes dans le dossier : %s\n\n" dest_dir

(** [load_network_from_url url target_date] est la fonction principale.
    Elle crée un dossier temporaire sur la machine, y télécharge et extrait les données via 
    [download_and_extract_gtfs], compile le graphe en mémoire avec [load_network_gtfs], 
    puis efface intelligemment les fichiers temporaires de l'ordinateur de l'utilisateur.
    
    @param url L'URL pointant vers le ZIP du GTFS.
    @param target_date La date de référence pour le calendrier.
    @return Le [TransportGraph] fini, stocké uniquement en RAM. *)
let load_network_from_url url target_date =
  let temp_base = Filename.get_temp_dir_name () in
  let temp_dir = Filename.concat temp_base "metrocaml_gtfs" in
  try
    download_and_extract_gtfs url temp_dir;
    let graph = load_network_gtfs temp_dir target_date in
    let is_win = Sys.os_type = "Win32" || Sys.os_type = "Cygwin" in
    let cmd_cleanup =
      if is_win then Printf.sprintf "rmdir /S /Q \"%s\"" temp_dir
      else Printf.sprintf "rm -rf \"%s\"" temp_dir
    in
    let _ = Sys.command cmd_cleanup in
    Logger.black "Jeu de données supprimé avec succès, graphe persistant\n";
    graph
  with
  | Invalid_GTFS_URL msg ->
      Logger.bold_red "\n[ERREUR CRITIQUE] Impossible de charger le réseau.\n";
      Logger.bold_red "Raison : %s\nURL testée : %s\n\n" msg url;
      raise (Invalid_GTFS_URL url)
  | e ->
      Logger.bold_red "\n[ERREUR] Un problème inattendu est survenu : %s\n"
        (Printexc.to_string e);
      raise e;
