open Graph_intf
open Weight_intf

open Heuristic_intf

(** A* Optimisé

    Ce module implémente l'algorithme de recherche de chemin A* via un
    Foncteur. Il est dérivé du module Dijkstra optimisé et en conserve toutes
    les spécificités :

    - Découplage Total : Il ignore la logique métier et délègue l'évaluation au module [WEIGHT].
    - State-Space Routing : Il ne route pas d'une "station" à une autre, mais
      d'un "état" à un autre (Station + Ligne) pour ne pas écraser les chemins
      valides.
    - Tas d'appariement : Il utilise un Tas d'Appariement garantissant des
      performances quasi-instantanées, même sur des graphes massifs.
    - Contrairement à Dijkstra, A* utilise une heuristique orientant son choix d'arête,
      fournie via le module [H] à la construction du foncteur. *)
module Make
  (G : GRAPH)
  (W : WEIGHT with type edge = G.Edge.t and type node = G.Node.t)
  (H : HEURISTIC with type node = G.Node.t)
= struct
  module P = Path.Make (G) (W)

  type path = P.t
  type graph = G.t

  (** Le Tas d'Appariement (Strictement identique à celui de Dijkstra) *)
  module TA = struct
    type state_tuple = G.Node.t * W.state_key

    type t =
      | Empty
      | Node of {
          f_cost : float;
          g_cost : W.t;
          node : G.Node.t;
          state : state_tuple;
          children : t list;
        }

    let empty = Empty

    let merge h1 h2 =
      match (h1, h2) with
      | Empty, h | h, Empty -> h
      | Node n1, Node n2 ->
          if n1.f_cost <= n2.f_cost then
            Node { n1 with children = h2 :: n1.children }
          else Node { n2 with children = h1 :: n2.children }

    let push ~f_cost ~g_cost node state h =
      merge (Node { f_cost; g_cost; node; state; children = [] }) h

    let merge_pairs lst =
      let rec pair acc = function
        | [] -> acc
        | [ h ] -> h :: acc
        | h1 :: h2 :: rest -> pair (merge h1 h2 :: acc) rest
      in
      List.fold_left merge Empty (pair [] lst)

    let pop = function
      | Empty -> None
      | Node n ->
          Some (n.f_cost, n.g_cost, n.node, n.state, merge_pairs n.children)
  end

  (** Trouve le chemin optimal de la source à la destination.
      @param start_time L'heure de départ (sert de poids initial).
      @param graph Le graphe pré-construit du réseau.
      @param source Le noeud de départ.
      @param destination Le noeud d'arrivée.
      @param transfer_allowed Fonction de vérification des correspondances. *)
  let find_path ~start_time ~(graph : G.t) ~(source : G.Node.t)
      ~(destination : G.Node.t) ~(transfer_allowed : G.Node.t -> bool) =
    if not (G.has_node graph source) then None
    else if not (G.has_node graph destination) then None
    else
      let initial_weight = W.start_at start_time in
      if G.Node.equal source destination then
        Some { P.steps = []; total_weight = initial_weight }
      else
        let n = G.size graph in
        let dist = Hashtbl.create (n * 2) in
        let prev = Hashtbl.create (n * 2) in

        let initial_state = (source, W.get_state_key initial_weight) in
        let g_scalar g = float_of_int (W.get_weight g) in

        (* La différence avec Dijkstra : on ajoute l'heuristique au f_cost *)
        let initial_f = g_scalar initial_weight +. H.estimate source destination in

        let ta = ref (TA.push ~f_cost:initial_f ~g_cost:initial_weight source initial_state TA.empty) in
        Hashtbl.add dist initial_state initial_weight;

        let rec process () =
          match TA.pop !ta with
          | None -> None
          | Some (_f_cost, g_cost, u, u_state, rest_ta) ->
              ta := rest_ta;
              let recorded_g = Option.value ~default:W.infinity (Hashtbl.find_opt dist u_state) in
              if W.(g_cost > recorded_g) then process ()
              else if G.Node.equal u destination then begin
                (* h(dest) = 0 pour toute heuristique admissible, donc f = g au dépilage.
                   Premier dépilage de la destination = coût global optimal. *)
                let rec build_steps acc state =
                  match Hashtbl.find_opt prev state with
                  | None -> acc
                  | Some (pred_state, edge, w, step_data) ->
                      let step =
                        { P.from = fst pred_state; to_node = fst state; edge; weight = w; data = step_data; }
                      in
                      build_steps (step :: acc) pred_state
                in
                Some { P.steps = build_steps [] u_state; total_weight = Hashtbl.find dist u_state; }
              end else begin
                List.iter
                  (fun (v, edge) ->
                    match W.traverse g_cost u edge with
                    | None -> ()
                    | Some (new_g, step_data) ->
                        let next_state_key = W.get_state_key new_g in
                        let _, current_state_key = u_state in
                        let is_line_change = current_state_key <> next_state_key in
                        let is_transfer = is_line_change && not (G.Node.equal u source) in

                        if (not is_transfer) || transfer_allowed u then (
                          let v_state = (v, next_state_key) in
                          let old_g = Option.value ~default:W.infinity (Hashtbl.find_opt dist v_state) in

                          if W.(new_g < old_g) then begin
                            (* Application de l'heuristique sur le nouveau noeud *)
                            let new_f = g_scalar new_g +. H.estimate v destination in
                            Hashtbl.replace dist v_state new_g;
                            Hashtbl.replace prev v_state (u_state, edge, new_g, step_data);
                            ta := TA.push ~f_cost:new_f ~g_cost:new_g v v_state !ta
                          end))
                  (G.neighbors graph u);
                process ()
              end
        in
        process ()
end
