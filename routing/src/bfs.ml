open Graph_intf
open Weight_intf

module Make (G : GRAPH) (W : WEIGHT with type edge = G.Edge.t) = struct
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
        let visited = Hashtbl.create (List.length (G.nodes graph)) in
        let prev = Hashtbl.create (List.length (G.nodes graph)) in
        let queue = Queue.create () in

        Hashtbl.add visited source ();
        Queue.add (source, initial_weight) queue;

        let rec bfs () =
          if Queue.is_empty queue then None
          else
            let current, current_weight = Queue.take queue in
            if G.Node.equal current destination then reconstruct_path ()
            else begin
              let neighbors = G.neighbors graph current in
              List.iter
                (fun (neighbor, edge) ->
                  if not (Hashtbl.mem visited neighbor) then begin
                    Hashtbl.add visited neighbor ();
                    let edge_weight = W.add current_weight edge in
                    Hashtbl.add prev neighbor (current, edge, edge_weight);
                    Queue.add (neighbor, edge_weight) queue
                  end)
                neighbors;
              bfs ()
            end
        and reconstruct_path () =
          let rec build_steps acc node =
            try
              let pred, edge, weight = Hashtbl.find prev node in
              let step = { P.from = pred; to_node = node; edge; weight } in
              build_steps (step :: acc) pred
            with Not_found -> acc
          in
          let steps = build_steps [] destination in
          let total_weight = 
            match List.rev steps with 
            | [] -> initial_weight 
            | last_step :: _ -> last_step.P.weight 
          in
          Some { P.steps; total_weight }
        in bfs ()
end