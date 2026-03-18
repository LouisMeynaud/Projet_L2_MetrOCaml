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

module Make (P : Routing.PATH) = struct
  let print_path node_to_string edge_to_string weight_to_string edge_to_color
      edge_to_duration edge_to_distance details (path : P.t) =
    match path.steps with
    | [] -> Printf.printf "Chemin vide\n"
    | first_step :: _ ->
        let start = node_to_string first_step.from in
        let stop = node_to_string (List.hd (List.rev path.steps)).to_node in
        let prev_line, prev_color, stops, transferts, segments =
          List.fold_left
            (fun (prev_line, prev_color, stops, transferts, acc) (step : P.step)
               ->
              let line = edge_to_string step.edge in
              let color = edge_to_color step.edge in
              match prev_line with
              | None ->
                  (* premier arrêt *)
                  (Some line, color, 1, transferts, acc)
              | Some l when l = line && stop = node_to_string step.to_node ->
                  (* Dernier arrêt *)
                  let arrow =
                    Printf.sprintf " --[%s | %d arrets]--> " l stops
                  in
                  let colored_arrow = colorize_hex prev_color arrow in
                  let segment =
                    Printf.sprintf "%s%s" colored_arrow
                      (node_to_string step.to_node)
                  in
                  (prev_line, color, 1, transferts, segment :: acc)
              | Some l when l = line ->
                  (* même ligne donc on accumule les arrêts *)
                  if details then
                    let arrow = Printf.sprintf " ---> " in
                    let colored_arrow = colorize_hex color arrow in
                    let segment =
                      Printf.sprintf "%s%s" colored_arrow
                        (node_to_string step.to_node)
                    in
                    (prev_line, color, stops + 1, transferts, segment :: acc)
                  else (prev_line, color, stops + 1, transferts, acc)
              | Some l ->
                  (* changement de ligne *)
                  let arrow =
                    Printf.sprintf " --[%s | %d arrets]--> " l stops
                  in
                  let colored_arrow = colorize_hex prev_color arrow in
                  let segment =
                    Printf.sprintf "%s%s\n%s" colored_arrow
                      (node_to_string step.to_node)
                      (node_to_string step.to_node)
                  in
                  (Some line, color, 1, transferts + 1, segment :: acc))
            (None, "", 0, 0, []) path.steps
        in
        (* Temps et Distance *)
        let total_time, total_distance =
          List.fold_left
            (fun (t, d) (step : P.step) ->
              (t + edge_to_duration step.edge, d +. edge_to_distance step.edge))
            (0, 0.0) path.steps
        in
        (*Affichage*)
        let route = "\n" ^ start ^ String.concat "" (List.rev segments) in
        Printf.printf "Chemin trouvé en %d étapes (en %d correspondances) :\n"
          (List.length path.steps) transferts;
        Printf.printf "%s\n" route;
        Printf.printf "Poids total : %s\n" (weight_to_string path.total_weight);
        Printf.printf "Temps total : %dh %dmin %ds\n" (total_time / 3600)
          (total_time / 60 mod 60)
          (total_time mod 60);
        Printf.printf "Distance totale : %f km\n" (total_distance /. 1000.)
end
