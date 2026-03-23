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
        let n = List.length (G.nodes graph) in
        let dist = Hashtbl.create n in
        let prev = Hashtbl.create n in
        let pq = ref [ (initial_weight, source) ] in
        Hashtbl.add dist source initial_weight;

        let pq_push w n =
          let rec insert = function
            | [] -> [ (w, n) ]
            | (w', n') :: rest ->
                if W.(w < w') then (w, n) :: (w', n') :: rest else (w', n') :: insert rest
          in pq := insert !pq
        in

        let rec process () =
          match !pq with
          | [] -> None
          | (d, u) :: rest ->
              pq := rest;
              if G.Node.equal u destination then
                let rec build_steps acc node =
                  match Hashtbl.find_opt prev node with
                  | None -> acc
                  | Some (pred, edge, w) ->
                      let step = { P.from = pred; to_node = node; edge; weight = w } in
                      build_steps (step :: acc) pred
                in
                Some { P.steps = build_steps [] destination; total_weight = Hashtbl.find dist destination }
              else if W.(d > Hashtbl.find dist u) then process ()
              else begin
                List.iter
                  (fun (v, edge) ->
                    let new_dist = W.add d edge in (* add prend directement l'arête ! *)
                    let old_dist = Option.value ~default:W.infinity (Hashtbl.find_opt dist v) in
                    if W.(new_dist < old_dist) then begin
                      Hashtbl.replace dist v new_dist;
                      Hashtbl.replace prev v (u, edge, new_dist); (* On stocke new_dist pour les steps *)
                      pq_push new_dist v
                    end)
                  (G.neighbors graph u);
                process ()
              end
        in process ()
end