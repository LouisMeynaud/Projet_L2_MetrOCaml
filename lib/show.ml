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
      (path : P.t) =
    match path.steps with
    | [] -> Printf.printf "Chemin vide\n"
    | first_step :: _ ->
        let start = node_to_string first_step.from in
        let _, _, transferts, segments =
          List.fold_left
            (fun (prev_line, stops, transferts, acc) (step : P.step) ->
              let line_name = edge_to_string step.edge in
              let color = edge_to_color step.edge in
              let segment, nb_stops, nb_transferts, new_prev_line =
                if Some line_name <> prev_line then
                  let arrow =
                    Printf.sprintf " --[%s | %d arrets]--> " line_name stops
                  in
                  let colored_arrow = colorize_hex color arrow in
                  ( Printf.sprintf "%s%s" colored_arrow
                      (node_to_string step.to_node),
                    1,
                    transferts + 1,
                    Some line_name )
                else ("", stops + 1, transferts, prev_line)
              in
              (new_prev_line, nb_stops, nb_transferts, acc @ [ segment ]))
            (None, 1, 0, []) path.steps
        in
        let route = start ^ String.concat "" segments in
        Printf.printf "Chemin trouvé en %d étapes (en %d correspondances) :\n"
          (List.length path.steps) transferts;
        Printf.printf "%s\n" route;
        Printf.printf "Poids total : %s\n" (weight_to_string path.total_weight)
end
