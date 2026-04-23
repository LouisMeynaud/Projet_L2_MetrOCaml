open Graph_intf
open Weight_intf

(** Algorithme de Parcours en Largeur (Breadth-First Search). Utile pour trouver
    le chemin avec le moins d'arêtes sans tenir compte des distances réelles. *)
module Make (G : GRAPH) (W : WEIGHT with type edge = G.Edge.t and type node = G.Node.t) = struct
  module P = Path.Make (G) (W)

  type path = P.t

  let find_path ~start_time ~graph ~source ~destination =
    if not (G.has_node graph source) then None
    else if not (G.has_node graph destination) then None
    else
      let initial_weight = W.start_at start_time in
      if G.Node.equal source destination then
        Some { P.steps = []; total_weight = initial_weight }
      else
        let visited = Hashtbl.create 10000 in
        let prev = Hashtbl.create 10000 in
        let queue = Queue.create () in
        let initial_state = (source, W.get_state_key initial_weight) in

        Hashtbl.add visited initial_state ();
        Queue.add (source, initial_state, initial_weight) queue;

        let rec bfs () =
          if Queue.is_empty queue then None
          else
            let current, current_state, current_weight = Queue.take queue in
            if G.Node.equal current destination then
              reconstruct_path current_state
            else begin
              let neighbors = G.neighbors graph current in
              List.iter
                (fun (neighbor, edge) ->
                  match W.traverse current_weight neighbor edge with
                  | None -> ()
                  | Some (edge_weight, step_data) ->
                      let neighbor_state =
                        (neighbor, W.get_state_key edge_weight)
                      in
                      (* On vérifie si l'état (Noeud + Ligne) a déjà été visité *)
                      if not (Hashtbl.mem visited neighbor_state) then begin
                        Hashtbl.add visited neighbor_state ();
                        Hashtbl.add prev neighbor_state
                          ( current_state,
                            current,
                            neighbor,
                            edge,
                            edge_weight,
                            step_data );
                        Queue.add (neighbor, neighbor_state, edge_weight) queue
                      end)
                neighbors;
              bfs ()
            end
        and reconstruct_path final_state =
          let rec build_steps acc state =
            try
              let pred_state, pred_node, current_node, edge, weight, step_data =
                Hashtbl.find prev state
              in
              let step =
                {
                  P.from = pred_node;
                  to_node = current_node;
                  edge;
                  weight;
                  data = step_data;
                }
              in
              build_steps (step :: acc) pred_state
            with Not_found -> acc
          in
          let steps = build_steps [] final_state in
          let total_weight =
            match List.rev steps with
            | [] -> initial_weight
            | last_step :: _ -> last_step.P.weight
          in
          Some { P.steps; total_weight }
        in
        bfs ()
end
