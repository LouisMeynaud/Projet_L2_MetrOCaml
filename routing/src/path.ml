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

  let print_path node_to_string path =
    Printf.printf "Chemin trouvé en %d étapes :\n" (List.length path.steps);
    Printf.printf " %s" (node_to_string (List.hd path.steps).from);
    List.iter (fun step ->
      Printf.printf " --> %s" (node_to_string step.to_node)
    ) path.steps;
    Printf.printf "\n"

end 