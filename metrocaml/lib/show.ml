open Routing
open Network

let station_indent = ""
let wait_indent = "  "
let line_indent = "     "

let hex_to_rgb hex =
  let hex =
    if String.length hex > 0 && hex.[0] = '#' then
      String.sub hex 1 (String.length hex - 1)
    else if hex = "" then "000000"
    else hex
  in
  let r = int_of_string ("0x" ^ String.sub hex 0 2) in
  let g = int_of_string ("0x" ^ String.sub hex 2 2) in
  let b = int_of_string ("0x" ^ String.sub hex 4 2) in
  (r, g, b)

let colorize_rgb use_color (r, g, b) text =
  if use_color then Printf.sprintf "\x1b[38;2;%d;%d;%dm%s\x1b[0m" r g b text
  else text

let colorize_hex use_color hex text =
  let rgb = hex_to_rgb hex in
  colorize_rgb use_color rgb text

let bold use_color text =
  if use_color then Printf.sprintf "\x1b[1m%s\x1b[0m" text else text

let italic use_color text =
  if use_color then Printf.sprintf "\x1b[3;90m%s\x1b[0m" text else text

let edge_label (c : connection) =
  match c.mode with
  | Walk | Transfert -> "Marche"
  | _ -> Printf.sprintf "%s %s" (string_of_mode c.mode) c.line.name

let normalize_seconds seconds =
  let day = 24 * 3600 in
  ((seconds mod day) + day) mod day

let format_hhmm seconds =
  let seconds = normalize_seconds seconds in
  Printf.sprintf "%02d:%02d" (seconds / 3600) (seconds mod 3600 / 60)

let line_minutes_from_seconds secs = if secs <= 0 then 0 else max 1 (secs / 60)

let format_station ~time ~station =
  Printf.sprintf "%s %s" (format_hhmm time) station

let format_wait ~wait_min = Printf.sprintf "Attente : %d min" wait_min

let format_ride ~stops ~minutes =
  Printf.sprintf "%d arrêts ~ %d min" stops minutes

let format_hms seconds =
  Printf.sprintf "%dh %dmin %ds" (seconds / 3600)
    (seconds / 60 mod 60)
    (seconds mod 60)

module Make (P : Routing.PATH with type Node.t = station and type Edge.t = connection) = struct
  let edge_line_key (c : connection) =
    Printf.sprintf "%s|%s" (string_of_mode c.mode) c.line.name

  type segment = { label : string; color : string; steps_rev : P.step list }

  type summary = {
    arrival_time : int;
    total_time : int;
    onboard_time : int;
    walking_time : int;
    waiting_time : int;
    total_distance : float;
    transfers : int;
  }

  let segment_is_walk (segment : segment) = String.equal segment.label "Marche"

  let segment_departure_time (segment : segment) ~arrival_time =
    match List.rev segment.steps_rev with
    | [] -> arrival_time
    | first_step :: _ ->
        max arrival_time
          (P.Weight.get_time first_step.weight - first_step.edge.duration)

  let compute_summary ~start_time (path : P.t) =
    let total_distance =
      List.fold_left
        (fun d (step : P.step) -> d +. step.edge.distance)
        0.0 path.steps
    in
    let rec loop prev_time prev_line_opt onboard walking waiting transfers =
      function
      | [] -> (onboard, walking, waiting, transfers)
      | (step : P.step) :: rest ->
          let step_end = P.Weight.get_time step.weight in
          let duration = step.edge.duration in
          let step_start = max prev_time (step_end - duration) in
          let wait_here = max 0 (step_start - prev_time) in
          let is_walk = Util.edge_is_walk step.edge in
          let onboard' = if is_walk then onboard else onboard + duration in
          let walking' = if is_walk then walking + duration else walking in
          let waiting' = waiting + wait_here in
          let transfers', prev_line_opt' =
            if is_walk then (transfers, prev_line_opt)
            else
              let current_line = edge_line_key step.edge in
              let transfers_delta =
                match prev_line_opt with
                | Some prev when not (String.equal prev current_line) -> 1
                | _ -> 0
              in
              (transfers + transfers_delta, Some current_line)
          in
          loop step_end prev_line_opt' onboard' walking' waiting' transfers' rest
    in
    let onboard_time, walking_time, waiting_time, transfers =
      loop start_time None 0 0 0 0 path.steps
    in
    let arrival_time = P.Weight.get_time path.total_weight in
    {
      arrival_time;
      total_time = max 0 (arrival_time - start_time);
      onboard_time;
      walking_time;
      waiting_time;
      total_distance;
      transfers;
    }

  let split_segments steps =
    let flush current acc =
      match current with None -> acc | Some segment -> segment :: acc
    in
    let rec loop current acc = function
      | [] -> List.rev (flush current acc)
      | (step : P.step) :: rest ->
          let label = edge_label step.edge in
          let color = step.edge.line.color in
          begin match current with
          | None -> loop (Some { label; color; steps_rev = [ step ] }) acc rest
          | Some segment ->
              if String.equal label segment.label then
                loop
                  (Some { segment with steps_rev = step :: segment.steps_rev })
                  acc rest
              else
                loop
                  (Some { label; color; steps_rev = [ step ] })
                  (segment :: acc) rest
          end
    in
    loop None [] steps

  let render_segment ~use_color ~arrival_time (segment : segment) =
    let steps = List.rev segment.steps_rev in
    match steps with
    | [] -> ("", arrival_time)
    | first_step :: _ ->
        let last_step = List.fold_left (fun _ step -> step) first_step steps in
        let departure_time = segment_departure_time segment ~arrival_time in
        let end_time = P.Weight.get_time last_step.weight in
        let ride_seconds = max 0 (end_time - departure_time) in
        let ride_minutes = line_minutes_from_seconds ride_seconds in
        let station_name = bold use_color first_step.from.name in
        let arrival_station = bold use_color last_step.to_node.name in
        let line_text =
          if segment_is_walk segment then italic use_color "Marche"
          else colorize_hex use_color segment.color segment.label
        in
        let lines =
          [
            station_indent
            ^ format_station ~time:arrival_time ~station:station_name;
            line_indent ^ line_text ^ " | "
            ^ format_ride ~stops:(List.length steps) ~minutes:ride_minutes;
            station_indent
            ^ format_station ~time:end_time ~station:arrival_station;
          ]
        in
        (String.concat "\n" lines, end_time)

  let render_route ~start_time ~use_color (path : P.t) =
    match path.steps with
    | [] -> ""
    | _ ->
        let segments = split_segments path.steps in
        let rec aux acc current_time = function
          | [] -> List.rev acc
          | [ segment ] ->
              let rendered, _end_time =
                render_segment ~use_color ~arrival_time:current_time segment
              in
              List.rev (rendered :: acc)
          | segment :: (next_segment :: _ as rest) ->
              let rendered, end_time =
                render_segment ~use_color ~arrival_time:current_time segment
              in
              let next_departure =
                segment_departure_time next_segment ~arrival_time:end_time
              in
              let wait_seconds = max 0 (next_departure - end_time) in
              let wait_minutes = line_minutes_from_seconds wait_seconds in
              let transition_line =
                if wait_seconds <= 0 then []
                else if segment_is_walk next_segment then
                  [ wait_indent ^ italic use_color "Marche" ]
                else
                  [ wait_indent ^ italic use_color (format_wait ~wait_min:wait_minutes) ]
              in
              let rendered_block =
                String.concat "\n" (rendered :: transition_line)
              in
              aux (rendered_block :: acc) end_time rest
        in
        let rendered_segments = aux [] start_time segments in
        String.concat "\n" rendered_segments

  let string_of_path ~start_time ~use_color (path : P.t) =
    let route = render_route ~start_time ~use_color path in
    let summary = compute_summary ~start_time path in
    match route with
    | "" -> "Chemin vide"
    | _ ->
        String.concat "\n"
          [
            route;
            "";
            Printf.sprintf "Arrivée : %s" (format_hhmm summary.arrival_time);
            Printf.sprintf "Temps total : %s" (format_hms summary.total_time);
            Printf.sprintf "Temps à bord : %s" (format_hms summary.onboard_time);
            Printf.sprintf "Temps de marche : %s" (format_hms summary.walking_time);
            Printf.sprintf "Temps d'attente : %s" (format_hms summary.waiting_time);
            Printf.sprintf "Distance totale : %.2f km" (summary.total_distance /. 1000.);
            Printf.sprintf "Correspondances : %d" summary.transfers;
          ]
end
