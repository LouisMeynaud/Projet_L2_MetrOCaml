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

let normalize_seconds seconds =
  let day = 24 * 3600 in
  ((seconds mod day) + day) mod day

let format_hhmm seconds =
  let seconds = normalize_seconds seconds in
  Printf.sprintf "%02d:%02d" (seconds / 3600) ((seconds mod 3600) / 60)

let line_minutes_from_seconds secs = if secs <= 0 then 0 else max 1 (secs / 60)

let format_station ~time ~station =
  Printf.sprintf "%s %s" (format_hhmm time) station

let format_wait ~wait_min = Printf.sprintf "Attente : %d min" wait_min
let format_ride ~stops ~minutes = Printf.sprintf "%d arrêts ~ %d min" stops minutes

module Make (P : Routing.PATH) = struct
  type segment = {
    label : string;
    color : string;
    steps_rev : P.step list;
  }

  let segment_is_walk (segment : segment) = String.equal segment.label "Marche"

  let segment_departure_time ~edge_to_duration (segment : segment) ~arrival_time =
    match List.rev segment.steps_rev with
    | [] -> arrival_time
    | first_step :: _ ->
        max arrival_time
          (P.Weight.get_time first_step.weight - edge_to_duration first_step.edge)

  let calcul_distance edge_to_distance steps =
    List.fold_left
      (fun d (step : P.step) -> d +. edge_to_distance step.edge)
      0.0 steps

  let split_segments edge_to_string edge_to_color steps =
    let flush current acc =
      match current with None -> acc | Some segment -> segment :: acc
    in
    let rec loop current acc = function
      | [] -> List.rev (flush current acc)
      | (step : P.step) :: rest ->
          let label = edge_to_string step.edge in
          let color = edge_to_color step.edge in
          begin
            match current with
            | None -> loop (Some { label; color; steps_rev = [ step ] }) acc rest
            | Some segment ->
                if String.equal label segment.label then
                  loop (Some { segment with steps_rev = step :: segment.steps_rev })
                    acc rest
                else
                  loop (Some { label; color; steps_rev = [ step ] })
                    (segment :: acc) rest
          end
    in
    loop None [] steps

  let render_segment ~use_color ~node_to_string ~edge_to_duration ~arrival_time
      (segment : segment) =
    let steps = List.rev segment.steps_rev in
    match steps with
    | [] -> ("", arrival_time, 0)
    | first_step :: _ ->
        let last_step = List.fold_left (fun _ step -> step) first_step steps in
        let departure_time =
          segment_departure_time ~edge_to_duration segment ~arrival_time
        in
        let end_time = P.Weight.get_time last_step.weight in
        let ride_seconds = max 0 (end_time - departure_time) in
        let ride_minutes = line_minutes_from_seconds ride_seconds in
        let station_name = bold use_color (node_to_string first_step.from) in
        let arrival_station = bold use_color (node_to_string last_step.to_node) in
        let line_text =
          if segment_is_walk segment then italic use_color "Marche"
          else colorize_hex use_color segment.color segment.label
        in
        let lines =
          [
            station_indent ^ format_station ~time:arrival_time ~station:station_name;
            line_indent ^ line_text ^ " | " ^ format_ride ~stops:(List.length steps) ~minutes:ride_minutes;
            station_indent ^ format_station ~time:end_time ~station:arrival_station;
          ]
        in
        (String.concat "\n" lines, end_time, ride_seconds)

  let get_chemin node_to_string edge_to_string _weight_to_string
      _weight_get_time edge_to_color edge_to_duration edge_to_distance start_time
      _details use_color (path : P.t) =
    match path.steps with
    | [] -> (0, "", 0, 0, 0.0)
    | _ ->
        let segments = split_segments edge_to_string edge_to_color path.steps in
        let transferts = max 0 (List.length segments - 1) in
        let rec render_route acc current_time onboard_total_secs = function
          | [] -> (List.rev acc, onboard_total_secs)
          | [ segment ] ->
              let rendered, end_time, ride_secs =
                render_segment ~use_color ~node_to_string ~edge_to_duration
                  ~arrival_time:current_time segment
              in
              ignore end_time;
              (List.rev (rendered :: acc), onboard_total_secs + ride_secs)
          | segment :: ((next_segment :: _) as rest) ->
              let rendered, end_time, ride_secs =
                render_segment ~use_color ~node_to_string ~edge_to_duration
                  ~arrival_time:current_time segment
              in
              let next_departure =
                segment_departure_time ~edge_to_duration next_segment
                  ~arrival_time:end_time
              in
              let wait_seconds = max 0 (next_departure - end_time) in
              let wait_minutes = line_minutes_from_seconds wait_seconds in
              let transition_line =
                if wait_seconds <= 0 then []
                else if segment_is_walk next_segment then
                  [ wait_indent ^ italic use_color "Marche" ]
                else
                  [
                    wait_indent
                    ^ italic use_color (format_wait ~wait_min:wait_minutes);
                  ]
              in
              let rendered_block =
                String.concat "\n" (rendered :: transition_line)
              in
              render_route (rendered_block :: acc) end_time
                (onboard_total_secs + ride_secs) rest
        in
        let rendered_segments, onboard_total_secs =
          render_route [] start_time 0 segments
        in
        let route = String.concat "\n" rendered_segments in
        let total_distance = calcul_distance edge_to_distance path.steps in
        let total_time = max 0 (P.Weight.get_time path.total_weight - start_time) in
        (transferts, route, total_time, onboard_total_secs, total_distance)

  let string_of_path node_to_string edge_to_string weight_to_string weight_get_time
      edge_to_color edge_to_duration edge_to_distance start_time details use_color
      (path : P.t) =
    let transferts, route, total_elapsed_time, onboard_total_secs, total_distance =
      get_chemin node_to_string edge_to_string weight_to_string weight_get_time
        edge_to_color edge_to_duration edge_to_distance start_time details
        use_color path
    in
    match route with
    | "" -> "Chemin vide"
    | _ ->
        String.concat "\n"
          [
            Printf.sprintf
              "Chemin trouvé en %d étapes (en %d correspondances) :"
              (List.length path.steps) transferts;
            route;
            Printf.sprintf "Poids total : %s" (weight_to_string path.total_weight);
            Printf.sprintf
              "Temps total estimé (attentes incluses) : %dh %dmin %ds"
              (total_elapsed_time / 3600)
              (total_elapsed_time / 60 mod 60)
              (total_elapsed_time mod 60);
            Printf.sprintf
              "Temps total passé dans les rames (arrêts intermédiaires inclus) : %dh %dmin %ds"
              (onboard_total_secs / 3600)
              (onboard_total_secs / 60 mod 60)
              (onboard_total_secs mod 60);
            Printf.sprintf "Distance totale : %f km" (total_distance /. 1000.);
          ]

  let print_path node_to_string edge_to_string weight_to_string weight_get_time
      edge_to_color edge_to_duration edge_to_distance start_time details use_color
      (path : P.t) =
    Printf.printf "%s\n"
      (string_of_path node_to_string edge_to_string weight_to_string
         weight_get_time edge_to_color edge_to_duration edge_to_distance
         start_time details use_color path)
end
