open Network

(** Moteur Physique et Modèles de Poids

    Ce fichier contient la logique métier (physique et temporelle) du réseau. Il
    gère la lecture des horaires, les pénalités de correspondances, les marges
    de tolérance, et le filtrage dynamique des modes de transport. *)

(** Temps de marche simulé (en secondes) ajouté lors d'un changement de ligne. *)
let transfer_penalty = 180

(** Temps de pénalité en cas de perturbations *)
let perturbations_penalty = 600

(** Liste globale des modes de transport désactivés par l'utilisateur. *)
let excluded_modes = ref []

let set_excluded_modes modes =
  excluded_modes :=
    List.map String.lowercase_ascii (List.map String.trim modes)
    |> List.filter (fun m -> m <> "")

let is_mode_excluded mode =
  let key = String.lowercase_ascii (string_of_mode mode) in
  List.mem key !excluded_modes

(** Outils de Formatage *)

(** Convertisseur du temps en secondes en string pour l'affichage *)
let time_to_string time =
  if time = max_int then "∞"
  else
    let days = time / 86400 in
    let hours = time mod 86400 / 3600 in
    let minutes = time mod 3600 / 60 in
    let seconds = time mod 60 in
    match days with
    | 0 -> Printf.sprintf "Aujourd'hui à %02dh%02dm%02ds" hours minutes seconds
    | 1 -> Printf.sprintf "Demain à %02dh%02dm%02ds" hours minutes seconds
    | 2 -> Printf.sprintf "Après-demain à %02dh%02dm%02ds" hours minutes seconds
    | _ -> Printf.sprintf "J+%d à %02dh%02dm%02ds" days hours minutes seconds

type metro_step_data = { wait_time : int }

let format_metro_step_data s =
  if s.wait_time > 0 then Printf.sprintf " [attente %d min]" (s.wait_time / 60)
  else ""

(** Évaluation Physique de l'Arête, renvoie l'heure d'arrivée à la station suivante, avec le temps
d'attente et la ligne empreintée du prochain train, ou None s'il n'y en a pas *)
let eval_edge current_time current_line_name (target_station : station) (edge : connection) =
  if current_time = max_int then None
  (** si c'est un walk ou un transfert, il n'y a pas d'horraire, on part directement *)
  else if edge.mode = Walk || edge.mode = Transfert then
    Some (current_time + edge.duration, 0, current_line_name)
  else
    (** on vérifie si on fait une correspondance ou non *)
    let is_transfer =
      match current_line_name with
      | Some l when l <> edge.line.name -> true
      | _ -> false
    in

    (** ajout d'une marge de -3 minutes à l'arrivée en gare pour être sûr de pouvoir rester dans 
    le même train (éviter des bugs liés à des arrondis dans le gtfs) *)
    let margin =
      match current_line_name with
      | None -> 0
      | Some l when l = edge.line.name -> 180
      | _ -> 0
    in

    begin
      (** vérification que la ligne n'est pas bloquée ou perturbée *)
      let is_blocked = ref false in
      let is_perturbed = ref false in

      let current_line_id = edge.line.id in 
      let target_station_id = target_station.id in 
      let cleaned_line_id = Gtfs_rt.nettoyer_id current_line_id in
      let cleaned_station_id = Gtfs_rt.nettoyer_id target_station_id in

      let index = Mutex.protect Gtfs_rt.mutex (fun () -> !Gtfs_rt.index_alertes_dijkstra) in
      begin match Hashtbl.find_opt index cleaned_line_id with
        | None -> () 
        | Some alertes_ligne ->
          List.iter (fun (alerte : Gtfs_rt.alerte_dijkstra) ->
            let concerne_station =
              Hashtbl.length alerte.stations_hash = 0 || 
              Hashtbl.mem alerte.stations_hash cleaned_station_id
            in
            
            if concerne_station then begin
              let est_en_cours =
                match alerte.periodes with
                | [] -> true 
                | ps -> List.exists (fun (p : Gtfs_rt.periode) -> current_time >= p.debut && current_time <= p.fin) ps
              in

              if est_en_cours then begin
                match alerte.severite with
                | Gtfs_rt.BLOQUANTE -> is_blocked := true
                | Gtfs_rt.PERTURBEE -> is_perturbed := true
              end
        end
      ) alertes_ligne
    end;

    (** Si la ligne est bloquée, on coupe la recherche *)
    if !is_blocked then None
    else 
      let perturbation_time =
        if !is_perturbed && (is_transfer || current_line_name = None) 
        then current_time + perturbations_penalty
        else current_time
      in
      let ready_time =
        if is_transfer then perturbation_time + transfer_penalty
        else perturbation_time - margin
      in

      (** recherche du prochain départ dans la liste des horaires *)
      let next_dep = 
        List.fold_left (fun meilleur_temps (_, t) ->
          if t >= ready_time && t < meilleur_temps then t 
          else meilleur_temps
        ) max_int edge.schedules
      in

      if next_dep = max_int then None
      else begin
        let actual_dep = max perturbation_time next_dep in
        let wait = max 0 (actual_dep - current_time) in
        Some (actual_dep + edge.duration, wait, Some edge.line.name)
      end
    end

(* ========================================================== *)
(* Les Modules de Poids                                       *)
(* ========================================================== *)

(** Module de poids pour le temps *)
module TimeWeight : Routing.WEIGHT with type edge = connection and type node = station = struct
  type t = { time : int; walk_time : int; line_name : string option }
  type edge = connection
  type node = station
  type step_data = metro_step_data
  type state_key = string option

  let start_at time = { time; walk_time = 0; line_name = None }
  let infinity = { time = max_int; walk_time = max_int; line_name = None }
  let is_inf w = w.time = max_int

  let traverse w (v : node) e =
    if is_mode_excluded e.mode then None
    else
      match eval_edge w.time w.line_name v e with
      | None -> None
      | Some (next_time, wait_time, next_line) ->
          let extra_walk_time =
            if e.mode = Walk || e.mode = Transfert then e.duration else 0
          in
          Some
            ( {
                time = next_time;
                walk_time = w.walk_time + extra_walk_time;
                line_name = next_line;
              },
              { wait_time } )

  let compare w1 w2 =
    if is_inf w1 && is_inf w2 then 0.0
    else if is_inf w1 then 1.0
    else if is_inf w2 then -1.0
    else
      let diff = w1.time - w2.time in
      if diff <> 0 then float_of_int diff /. 3600.0
      else float_of_int (w1.walk_time - w2.walk_time) /. 3600.0

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_weight w = w.time
  let to_string w = time_to_string w.time
  let step_data_to_string = format_metro_step_data
  let get_state_key w = w.line_name
end

(** Module de poids pour la distance *)
module DistanceWeight : Routing.WEIGHT with type edge = connection and type node = station = struct
  type t = { time : int; distance : float; line_name : string option }
  type edge = connection
  type node = station
  type step_data = metro_step_data
  type state_key = string option

  let start_at time = { time; distance = 0.0; line_name = None }
  let infinity = { time = max_int; distance = Float.infinity; line_name = None }
  let is_inf w = w.time = max_int

  let traverse w (v : node) e =
    if is_mode_excluded e.mode then None
    else
      match eval_edge w.time w.line_name v e with
      | None -> None
      | Some (next_time, wait_time, next_line) ->
          let new_w =
            {
              time = next_time;
              distance = w.distance +. e.distance;
              line_name = next_line;
            }
          in
          Some (new_w, { wait_time })

  let compare w1 w2 =
    if is_inf w1 && is_inf w2 then 0.0
    else if is_inf w1 then 1.0
    else if is_inf w2 then -1.0
    else
      let diff_transfers = w1.distance -. w2.distance in
      diff_transfers /. 50000.0

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_weight w = w.time

  let to_string w =
    if is_inf w then "∞"
    else if w.distance >= 1000.0 then
      Printf.sprintf "%.1f km (%s)" (w.distance /. 1000.0)
        (time_to_string w.time)
    else Printf.sprintf "%.0f m (%s)" w.distance (time_to_string w.time)

  let step_data_to_string = format_metro_step_data
  let get_state_key w = w.line_name
end

(** Module de poids pour le nombre de stations *)
module HopWeight : Routing.WEIGHT with type edge = connection and type node = station = struct
  type t = { time : int; hops : int; line_name : string option }
  type edge = connection
  type node = station
  type step_data = metro_step_data
  type state_key = string option

  let start_at time = { time; hops = 0; line_name = None }
  let infinity = { time = max_int; hops = max_int; line_name = None }
  let is_inf w = w.time = max_int

  let traverse w (v : node) e =
    if is_mode_excluded e.mode then None
    else
      match eval_edge w.time w.line_name v e with
      | None -> None
      | Some (next_time, wait_time, next_line) ->
          let new_w =
            { time = next_time; hops = w.hops + 1; line_name = next_line }
          in
          Some (new_w, { wait_time })

  let compare w1 w2 =
    if is_inf w1 && is_inf w2 then 0.0
    else if is_inf w1 then 1.0
    else if is_inf w2 then -1.0
    else
      float_of_int (w1.hops - w2.hops) /. 40.0

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_weight w = w.time

  let to_string w =
    if is_inf w then "∞"
    else
      let txt = if w.hops <= 1 then "arrêt" else "arrêts" in
      Printf.sprintf "%d %s (%s)" w.hops txt (time_to_string w.time)

  let step_data_to_string = format_metro_step_data
  let get_state_key w = w.line_name
end

(** Module de poids pour le nombre de correspondances *)
module CorrespondenceWeight : Routing.WEIGHT with type edge = connection and type node = station = struct
  type t = { time : int; transfers : int; line_name : string option }
  type edge = connection
  type node = station
  type step_data = metro_step_data
  type state_key = string option

  let start_at time = { time; transfers = 0; line_name = None }
  let infinity = { time = max_int; transfers = max_int; line_name = None }
  let is_inf w = w.time = max_int

  let traverse w (v : node) e =
    if is_mode_excluded e.mode then None
    else
      match eval_edge w.time w.line_name v e with
      | None -> None
      | Some (next_time, wait_time, next_line) ->
          let is_transfer =
            match w.line_name with
            | Some l
              when e.mode <> Walk && e.mode <> Transfert && l <> e.line.name ->
                1
            | _ -> 0
          in
          let new_w =
            {
              time = next_time;
              transfers = w.transfers + is_transfer;
              line_name = next_line;
            }
          in
          Some (new_w, { wait_time })

  let compare w1 w2 =
    if is_inf w1 && is_inf w2 then 0.0
    else if is_inf w1 then 1.0
    else if is_inf w2 then -1.0
    else
      float_of_int (w1.transfers - w2.transfers) /. 5.0

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_weight w = w.time

  let to_string w =
    if is_inf w then "∞"
    else
      let txt = if w.transfers <= 1 then "correspondance" else "correspondances" in
      Printf.sprintf "%d %s (%s)" w.transfers txt (time_to_string w.time)

  let step_data_to_string = format_metro_step_data
  let get_state_key w = w.line_name
end