open Network

let transfer_penalty = 180
let transfer_time_tradeoff = 900

let excluded_modes = ref []

let set_excluded_modes modes =
  excluded_modes :=
    List.map String.lowercase_ascii (List.map String.trim modes)
    |> List.filter (fun m -> m <> "")

let is_mode_excluded mode =
  let key = String.lowercase_ascii (string_of_mode mode) in
  List.mem key !excluded_modes

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

let eval_edge current_time current_line_name (edge : connection) =
  if current_time = max_int then None
  else if edge.mode = Walk || edge.mode = Transfert then
    Some (current_time + edge.duration, 0, current_line_name)
  else
    let is_transfer =
      match current_line_name with
      | Some l when l <> edge.line.name -> true
      | _ -> false
    in

    let margin =
      match current_line_name with
      | None -> 0
      | Some l when l = edge.line.name -> 180
      | _ -> 0
    in

    let ready_time =
      if is_transfer then current_time + transfer_penalty
      else current_time - margin
    in

    let next_dep =
      List.fold_left
        (fun best t -> if t >= ready_time && t < best then t else best)
        max_int edge.schedules
    in

    if next_dep = max_int then None
    else
      let actual_dep = max current_time next_dep in
      let wait = max 0 (next_dep - current_time) in

      Some (actual_dep + edge.duration, wait, Some edge.line.name)

(* ========================================================== *)
(* LES MODULES DE POIDS *)
(* ========================================================== *)

module TimeWeight : Routing.WEIGHT with type edge = connection = struct
  type t = { time : int; line_name : string option }
  type edge = connection
  type step_data = metro_step_data
  type state_key = string option

  let start_at time = { time; line_name = None }
  let infinity = { time = max_int; line_name = None }
  let is_inf w = w.time = max_int

  let traverse w e =
    if is_mode_excluded e.mode then None
    else
      match eval_edge w.time w.line_name e with
    | None -> None
    | Some (next_time, wait_time, next_line) ->
        Some ({ time = next_time; line_name = next_line }, { wait_time })

  let compare w1 w2 =
    if is_inf w1 && is_inf w2 then 0.0
    else if is_inf w1 then 1.0
    else if is_inf w2 then -1.0
    else
      let diff = w1.time - w2.time in
      float_of_int diff /. float_of_int (Int.max 1 (Int.max w1.time w2.time))

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_time w = w.time
  let to_string w = time_to_string w.time
  let step_data_to_string = format_metro_step_data
  let get_state_key w = w.line_name
end

module DistanceWeight : Routing.WEIGHT with type edge = connection = struct
  type t = { time : int; distance : float; line_name : string option }
  type edge = connection
  type step_data = metro_step_data
  type state_key = string option

  let start_at time = { time; distance = 0.0; line_name = None }
  let infinity = { time = max_int; distance = Float.infinity; line_name = None }
  let is_inf w = w.time = max_int

  let traverse w e =
    if is_mode_excluded e.mode then None
    else
      match eval_edge w.time w.line_name e with
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
      let diff = w1.distance -. w2.distance in
      if Float.abs diff < 0.1 then
        float_of_int (w1.time - w2.time)
        /. float_of_int (Int.max 1 (Int.max w1.time w2.time))
      else diff /. Float.max 1.0 (Float.max w1.distance w2.distance)

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_time w = w.time

  let to_string w =
    if is_inf w then "∞"
    else if w.distance >= 1000.0 then
      Printf.sprintf "%.1f km (%s)" (w.distance /. 1000.0)
        (time_to_string w.time)
    else Printf.sprintf "%.0f m (%s)" w.distance (time_to_string w.time)

  let step_data_to_string = format_metro_step_data
  let get_state_key w = w.line_name
end

module HopWeight : Routing.WEIGHT with type edge = connection = struct
  type t = { time : int; hops : int; line_name : string option }
  type edge = connection
  type step_data = metro_step_data
  type state_key = string option

  let start_at time = { time; hops = 0; line_name = None }
  let infinity = { time = max_int; hops = max_int; line_name = None }
  let is_inf w = w.time = max_int

  let traverse w e =
    if is_mode_excluded e.mode then None
    else
      match eval_edge w.time w.line_name e with
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
      let diff = w1.hops - w2.hops in
      if diff = 0 then
        float_of_int (w1.time - w2.time)
        /. float_of_int (Int.max 1 (Int.max w1.time w2.time))
      else
        float_of_int diff /. float_of_int (Int.max 1 (Int.max w1.hops w2.hops))

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_time w = w.time

  let to_string w =
    if is_inf w then "∞"
    else Printf.sprintf "%d arrêts (%s)" w.hops (time_to_string w.time)

  let step_data_to_string = format_metro_step_data
  let get_state_key w = w.line_name
end

module CorrespondenceWeight : Routing.WEIGHT with type edge = connection =
struct
  type t = { time : int; transfers : int; line_name : string option }
  type edge = connection
  type step_data = metro_step_data
  type state_key = string option

  let start_at time = { time; transfers = 0; line_name = None }
  let infinity = { time = max_int; transfers = max_int; line_name = None }
  let is_inf w = w.time = max_int

  let traverse w e =
    if is_mode_excluded e.mode then None
    else
      match eval_edge w.time w.line_name e with
    | None -> None
    | Some (next_time, wait_time, next_line) ->
        let is_transfer =
          match w.line_name with
          | Some l when e.mode <> Walk && e.mode <> Transfert && l <> e.line.name -> 1
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
      let cost1 = (w1.transfers * transfer_time_tradeoff) + w1.time in
      let cost2 = (w2.transfers * transfer_time_tradeoff) + w2.time in
      float_of_int (cost1 - cost2) /. float_of_int (Int.max 1 (Int.max cost1 cost2))

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_time w = w.time

  let to_string w =
    if is_inf w then "∞"
    else
      Printf.sprintf "%d correspondances (%s)" w.transfers
        (time_to_string w.time)

  let step_data_to_string = format_metro_step_data
  let get_state_key w = w.line_name
end
