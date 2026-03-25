open Routing
open Network

(* Functions of conversion of colors *)
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

let colorize_rgb (r, g, b) text =
  Printf.sprintf "\x1b[38;2;%d;%d;%dm%s\x1b[0m" r g b text

let colorize_hex hex text =
  let rgb = hex_to_rgb hex in
  colorize_rgb rgb text

let change_arrow node_name line nb_stops temps color =
  let arrow = Printf.sprintf " --[%s | %d arrets | %d min] --> " line nb_stops temps in
  let colored_arrow = colorize_hex color arrow in
  Printf.sprintf "%s%s\n" colored_arrow node_name

let simple_arrow node_name color =
  let arrow = Printf.sprintf " ---> " in
  let colored_arrow = colorize_hex color arrow in
  Printf.sprintf "%s%s" colored_arrow node_name

module Make (P : Routing.PATH) = struct
  let line_minutes_from_seconds secs =
    if secs <= 0 then 0 else max 1 (secs / 60)

  let calcul_temps_distance edge_to_duration edge_to_distance steps =
    List.fold_left
      (fun (t, d) (step : P.step) ->
        (t + edge_to_duration step.edge, d +. edge_to_distance step.edge))
      (0, 0.0) steps

  let print_path node_to_string edge_to_string weight_to_string weight_get_time
      edge_to_color edge_to_duration edge_to_distance start_time details
      (path : P.t) =
    match path.steps with
    | [] -> Printf.printf "Chemin vide\n"
    | first_step :: _ ->
        let first_arrival = weight_get_time first_step.weight in
        let first_duration = edge_to_duration first_step.edge in
        let first_wait = first_arrival - first_duration - start_time in
        let first_wait_str =
          if first_wait > 0 then
            Printf.sprintf " [attente %d min]" (first_wait / 60)
          else ""
        in
        let start = node_to_string first_step.from ^ first_wait_str in

        let _, final_step_to, final_color, final_stops, final_onboard_secs,
            transferts, _, segments
            =
          List.fold_left
            (fun ( prev_line,
                   prev_step_to,
                   prev_color,
                   stops,
                   onboard_secs,
                   transferts,
                   prev_time,
                   acc ) (step : P.step) ->
              let current_line = edge_to_string step.edge in
              let color = edge_to_color step.edge in
              let step_to = node_to_string step.to_node in

              let current_arrival = weight_get_time step.weight in
              let duration = edge_to_duration step.edge in
              let wait_time = current_arrival - duration - prev_time in

              match prev_line with
              | None ->
                  (* Tout premier arrêt du trajet *)
                  ( Some current_line,
                    step_to,
                    color,
                    1,
                    duration,
                    transferts,
                    current_arrival,
                    acc )
              | Some prev_l when prev_l = current_line ->
                  let new_acc =
                    if details then
                      simple_arrow (node_to_string step.from) color :: acc
                    else acc
                  in
                  ( Some current_line,
                    step_to,
                    color,
                    stops + 1,
                    onboard_secs + duration,
                    transferts,
                    current_arrival,
                    new_acc )
              | Some prev_l ->
                  let wait_str =
                    if wait_time > 0 then
                      Printf.sprintf " [attente %d min]" (wait_time / 60)
                    else ""
                  in
                  let onboard_time = line_minutes_from_seconds onboard_secs in
                  let arrow =
                    change_arrow prev_step_to prev_l stops onboard_time prev_color
                  in
                  let transition =
                    arrow ^ node_to_string step.from ^ wait_str
                  in
                  ( Some current_line,
                    step_to,
                    color,
                    1,
                      duration,
                    transferts + 1,
                    current_arrival,
                    transition :: acc ))
                    (None, "", "", 0, 0, 0, start_time, [])
            path.steps
        in

        let final_line = edge_to_string (List.hd (List.rev path.steps)).edge in
        let final_segment =
          change_arrow final_step_to final_line final_stops (line_minutes_from_seconds final_onboard_secs) final_color
        in

        let total_time, total_distance =
          calcul_temps_distance edge_to_duration edge_to_distance path.steps
        in

        let route =
          "\n" ^ start ^ String.concat "" (List.rev (final_segment :: segments))
        in
        Printf.printf "Chemin trouvé en %d étapes (en %d correspondances) :\n"
          (List.length path.steps) transferts;
        Printf.printf "%s\n" route;
        Printf.printf "Poids total : %s\n" (weight_to_string path.total_weight);
        Printf.printf "Temps total passé dans les rames : %dh %dmin %ds\n"
          (total_time / 3600)
          (total_time / 60 mod 60)
          (total_time mod 60);
        Printf.printf "Distance totale : %f km\n" (total_distance /. 1000.)

  let print_path_detail node_to_string edge_to_string weight_to_string
      edge_to_color edge_to_duration edge_to_distance (path : P.t) =
    let weight_get_time (_ : P.Weight.t) = 0 in
    print_path node_to_string edge_to_string weight_to_string weight_get_time
      edge_to_color edge_to_duration edge_to_distance 0 true path
end
