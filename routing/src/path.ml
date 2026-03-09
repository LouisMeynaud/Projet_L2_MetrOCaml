(** Path module

Generic path that represent routes through any graph with any weight system **)

open Path_intf
open Graph_intf


module Make (Graph : GRAPH) (Weight : Weight_intf.WEIGHT with type edge = Graph.Edge.t) : PATH  
  with type node = Graph.Node.t
  and type edge = Graph.Edge.t
  and type weight = Weight.t = struct

  type node = Graph.Node.t
  type edge = Graph.Edge.t
  type weight = Weight.t 

  type step = {
    from : node;
    to_node : node;
    edge : edge;
    weight : weight;
  }

  type t = {
    steps : step list;
    total_weight : weight;
  }

  (** For use the function to convert weight to string *)
  module Weight = struct
    let to_string = Weight.to_string
  end
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

  let print_path node_to_string edge_to_string weight_to_string path =
  match path.steps with
  | [] -> Printf.printf "Chemin vide\n"
  | first_step :: _ ->
      let start = node_to_string first_step.from in
      let (_, segments) =
        List.fold_left
          (fun (prev_line, acc) step ->
            let line_name = edge_to_string step.edge in
            let segment, new_prev_line =
            
              if Some line_name <> prev_line then
                (Printf.sprintf " --[%s]--> %s"
                   line_name
                   (node_to_string step.to_node),
                 Some line_name)
              else
                (Printf.sprintf " --> %s"
                   (node_to_string step.to_node),
                 prev_line)
            in

            (new_prev_line, acc @ [segment])

          )
          (None, [])
          path.steps
      in

      let route = start ^ String.concat "" segments in

      Printf.printf "Chemin trouvé en %d étapes :\n" (List.length path.steps);
      Printf.printf "%s\n" route;
      Printf.printf "Poids total : %s\n" (weight_to_string path.total_weight)

end 