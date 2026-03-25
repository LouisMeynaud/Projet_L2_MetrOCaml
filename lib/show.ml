open Routing
open Network

(*    
  let print_path node_to_string edge_to_string weight_to_string path =
    Printf.printf "Chemin trouvé en %d étapes :\n" (List.length path.steps);
    Printf.printf " %s" (node_to_string (List.hd path.steps).from);
    List.iter (fun step ->
      Printf.printf " --[%s]--> %s" 
        (edge_to_string step.edge)
        (node_to_string step.to_node)
    ) path.steps;
    Printf.printf "\nPoids total : %s\n" (weight_to_string path.total_weight)
  *)

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

let change_arrow node_name line nb_stops color =
  let arrow = Printf.sprintf " --[%s | %d arrets] --> " line nb_stops in
  let colored_arrow = colorize_hex color arrow in
  let final_arrow = Printf.sprintf "%s%s\n" colored_arrow node_name in
  final_arrow

let simple_arrow node_name color =
  let arrow = Printf.sprintf " ---> " in
  let colored_arrow = colorize_hex color arrow in
  let final_arrow = Printf.sprintf "%s%s" colored_arrow node_name in
  final_arrow

module Make (P : Routing.PATH) = struct
  let calcul_temps_distance edge_to_duration edge_to_distance steps =
    List.fold_left
      (fun (t, d) (step : P.step) ->
        (t + edge_to_duration step.edge, d +. edge_to_distance step.edge))
      (0, 0.0) steps

  let init node_to_string (steps : P.step list) =
    ( node_to_string (List.hd steps).from,
      node_to_string (List.hd (List.rev steps)).to_node )

  let affichage start chemin length transferts total_time total_distance
      total_weight =
    (*Affichage*)
    let route = "\n" ^ start ^ String.concat "" (List.rev chemin) in
    Printf.printf "Chemin trouvé en %d étapes (en %d correspondances) :\n"
      length transferts;
    Printf.printf "%s\n" route;
    Printf.printf "Poids total : %s\n" total_weight;
    Printf.printf "Temps total : %dh %dmin %ds\n" (total_time / 3600)
      (total_time / 60 mod 60)
      (total_time mod 60);
    Printf.printf "Distance totale : %f km\n" (total_distance /. 1000.)

  let print_path node_to_string edge_to_string weight_to_string edge_to_color
      edge_to_duration edge_to_distance details (path : P.t) =
    match path.steps with
    | [] -> Printf.printf "Chemin vide\n"
    | first_step :: _ ->
        let start, stop = init node_to_string path.steps in
        let _, _, _, stops, transferts, segments =
          List.fold_left
            (fun (prev_line, prev_step_to, prev_color, stops, transferts, acc)
                 (step : P.step) ->
              let current_line = edge_to_string step.edge in
              let color = edge_to_color step.edge in
              let step_to = node_to_string step.to_node in
              match prev_line with
              (* premier arrêt *)
              | None -> (Some current_line, step_to, color, 1, transferts, acc)
              (* Dernier arrêt *)
              | Some prev_l when stop = node_to_string step.to_node ->
                  let from_arrow, arret =
                    if details then
                      if prev_l = current_line then
                        (simple_arrow (node_to_string step.from) color, stops)
                      else
                        ( change_arrow prev_step_to prev_l stops prev_color
                          ^ node_to_string step.from,
                          0 )
                    else ("", stops)
                  in
                  let to_arrow =
                    change_arrow
                      (node_to_string step.to_node)
                      current_line (arret + 1) color
                  in

                  ( Some current_line,
                    step_to,
                    color,
                    1,
                    transferts,
                    (from_arrow ^ to_arrow) :: acc )
              (*même ligne donc on accumule les arrêts *)
              | Some prev_l when prev_l = current_line ->
                  if details then
                    let arrow = simple_arrow (node_to_string step.from) color in
                    ( Some current_line,
                      step_to,
                      color,
                      stops + 1,
                      transferts,
                      arrow :: acc )
                  else
                    ( Some current_line,
                      step_to,
                      color,
                      stops + 1,
                      transferts,
                      acc )
              (* changement de ligne *)
              | Some prev_l ->
                  let arrow =
                    change_arrow prev_step_to prev_l stops prev_color
                  in
                  ( Some current_line,
                    step_to,
                    color,
                    1,
                    transferts + 1,
                    (arrow ^ node_to_string step.from) :: acc ))
            (None, "", "", 0, 0, []) path.steps
        in
        (* Temps et Distance *)
        let total_time, total_distance =
          calcul_temps_distance edge_to_duration edge_to_distance path.steps
        in
        (*Affichage*)
        affichage start segments (List.length path.steps) transferts total_time
          total_distance
          (weight_to_string path.total_weight)

  let print_path_detail node_to_string edge_to_string weight_to_string
      edge_to_color edge_to_duration edge_to_distance (path : P.t) =
    print_path node_to_string edge_to_string weight_to_string edge_to_color
      edge_to_duration edge_to_distance true path
end
