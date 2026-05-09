open Graph_intf
open Weight_intf

(** Algorithme de Parcours en Largeur (Breadth-First Search). Utile pour trouver
    le chemin avec le moins d'arêtes sans tenir compte des distances réelles. *)
module Make (G : GRAPH) (W : WEIGHT with type edge = G.Edge.t and type node = G.Node.t) = struct
  module P = Path.Make (G) (W)

  type path = P.t
  type graph = G.t

  let find_path ~start_time ~(graph : G.t) ~(source : G.Node.t)
      ~(destination : G.Node.t) ~(transfer_allowed : G.Node.t -> bool) =
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
            let u, u_state, u_weight = Queue.take queue in
            if G.Node.equal u destination then
              reconstruct_path u_state
            else begin
              List.iter
                (fun (v, edge) ->
                  match W.traverse u_weight u edge with
                  | None -> ()
                  | Some (new_weight, step_data) ->
                      let next_state_key = W.get_state_key new_weight in
                      let _, current_state_key = u_state in
                      let is_line_change = Stdlib.compare current_state_key next_state_key <> 0 in
                      let is_transfer = is_line_change && not (G.Node.equal u source) in
                      if (not is_transfer) || transfer_allowed u then begin
                        let v_state = (v, next_state_key) in
                        (* On vérifie si l'état (Noeud + Ligne) a déjà été visité *)
                        if not (Hashtbl.mem visited v_state) then begin
                          Hashtbl.add visited v_state ();
                          Hashtbl.add prev v_state (u_state, u, v, edge, new_weight, step_data);
                          Queue.add (v, v_state, new_weight) queue
                        end
                      end)
                (G.neighbors graph u);
              bfs ()
            end
        and reconstruct_path final_state =
          let rec build_steps acc state =
            match Hashtbl.find_opt prev state with
            | None -> acc
            | Some (pred_state, pred_node, current_node, edge, weight, step_data) ->
                let step =
                  { P.from = pred_node; to_node = current_node; edge; weight; data = step_data }
                in
                build_steps (step :: acc) pred_state
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
