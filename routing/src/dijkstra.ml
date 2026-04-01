open Graph_intf
open Weight_intf

module Make (G : GRAPH) (W : WEIGHT with type edge = G.Edge.t) = struct
  module P = Path.Make (G) (W)

  type path = P.t
  
  module PQ = struct
    type state_tuple = G.Node.t * W.state_key
    type t = Empty | Node of W.t * G.Node.t * state_tuple * t list

    let empty = Empty

    let merge h1 h2 =
      match h1, h2 with
      | Empty, h | h, Empty -> h
      | Node (w1, n1, s1, l1), Node (w2, n2, s2, l2) ->
          if W.(w1 < w2) then Node (w1, n1, s1, h2 :: l1)
          else Node (w2, n2, s2, h1 :: l2)

    let push w n s h = merge (Node (w, n, s, [])) h

    let rec merge_pairs = function
      | [] -> Empty
      | [h] -> h
      | h1 :: h2 :: rest -> merge (merge h1 h2) (merge_pairs rest)

    let pop = function
      | Empty -> None
      | Node (w, n, s, l) -> Some (w, n, s, merge_pairs l)
  end

  let find_path ~start_time ~graph ~source ~destination =
    if not (G.has_node graph source) then None
    else if not (G.has_node graph destination) then None
    else
      let initial_weight = W.start_at start_time in
      if G.Node.equal source destination then
        Some { P.steps = []; total_weight = initial_weight }
      else
        let dist = Hashtbl.create 10000 in
        let prev = Hashtbl.create 10000 in
        
        let initial_state = (source, W.get_state_key initial_weight) in
        
        let pq = ref (PQ.push initial_weight source initial_state PQ.empty) in
        Hashtbl.add dist initial_state initial_weight;

        let rec process () =
          match PQ.pop !pq with
          | None -> None
          | Some (d, u, u_state, rest_pq) ->
              pq := rest_pq;
              if G.Node.equal u destination then
                let rec build_steps acc state =
                  match Hashtbl.find_opt prev state with
                  | None -> acc
                  | Some (pred_state, pred_node, current_node, edge, w, step_data) ->
                      let step =
                        { P.from = pred_node; to_node = current_node; edge; weight = w; data = step_data }
                      in
                      build_steps (step :: acc) pred_state
                in
                Some
                  {
                    P.steps = build_steps [] u_state;
                    total_weight = Hashtbl.find dist u_state;
                  }
              else if W.(d > Hashtbl.find dist u_state) then process ()
              else begin
                List.iter
                  (fun (v, edge) ->
                    match W.traverse d edge with
                    | None -> ()
                    | Some (new_dist, step_data) ->
                        let v_state = (v, W.get_state_key new_dist) in
                        let old_dist = Option.value ~default:W.infinity (Hashtbl.find_opt dist v_state) in
                        if W.(new_dist < old_dist) then begin
                          Hashtbl.replace dist v_state new_dist;
                          Hashtbl.replace prev v_state (u_state, u, v, edge, new_dist, step_data);
                          
                          pq := PQ.push new_dist v v_state !pq
                        end)
                  (G.neighbors graph u);
                process ()
              end
        in
        process ()
end