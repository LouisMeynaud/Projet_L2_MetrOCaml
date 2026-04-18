open Graph_intf
open Weight_intf

(** A* Optimisé

    Ce module implémente l'algorithme de recherche de chemin A* via un
    Foncteur. Il est dérivé du module Dijkstra optimisé et en conserve toutes
    les spécificités :

    - Découplage Total : Il ignore la logique métier (métro, horaires) et
      délègue l'évaluation au module [WEIGHT].
    - State-Space Routing : Il ne route pas d'une "station" à une autre, mais
      d'un "état" à un autre (Station + Ligne) pour ne pas écraser les chemins
      valides.
    - Pairing Heap : Il utilise un Tas d'Appariement garantissant des
      performances quasi-instantanées, même sur des graphes massifs comme celui
      de Paris.

    La différence fondamentale avec Dijkstra est l'introduction d'une
    heuristique admissible [h : node -> node -> float] passée à [find_path].

    Le tas est ordonné par [f = g_scalar + h], où :
    - [g_scalar] est la projection scalaire de [W.t] via [W.get_time], servant
      de base de comparaison homogène avec [h].
    - [h] est l'estimation du coût restant, exprimée dans la même unité.

    Seul [g] (le poids réel [W.t]) est stocké dans [dist] et [prev], ce qui
    garantit l'exactitude du chemin reconstruit indépendamment de [h].

    Admissibilité : Pour que A* retourne un chemin optimal, [h] ne doit jamais
    surestimer le coût réel restant. Une heuristique nulle [fun _ _ -> 0.0]
    dégénère exactement en Dijkstra. *)
module Make (G : GRAPH) (W : WEIGHT with type edge = G.Edge.t) = struct
  module P = Path.Make (G) (W)

  type path = P.t

  (** Le Tas d'Appariement (Pairing Heap)

      Deux coûts sont séparés dans chaque nœud du tas :
      - [f_cost : float] : priorité de tri dans le tas (= g_scalar + h).
        C'est un flottant pour être homogène avec la valeur retournée par
        l'heuristique, quelle que soit la représentation interne de [W.t].
      - [g_cost : W.t] : coût réel accumulé depuis la source. C'est la
        valeur canonique stockée dans [dist] et [prev]. *)
  module PQ = struct
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

    let rec merge_pairs = function
      | [] -> Empty
      | [ h ] -> h
      | h1 :: h2 :: rest -> merge (merge h1 h2) (merge_pairs rest)

    let pop = function
      | Empty -> None
      | Node n ->
          Some (n.f_cost, n.g_cost, n.node, n.state, merge_pairs n.children)
  end

  (** Trouve le chemin optimal de la [source] à la [destination].
      @param start_time L'heure de départ (sert de poids initial).
      @param graph Le graphe pré-construit du réseau.
      @param source Le nœud de départ.
      @param destination Le nœud d'arrivée.
      @param transfer_allowed Fonction de vérification des correspondances.
      @param heuristic Fonction [h : node -> node -> float] estimant le coût
             restant entre un nœud courant et la destination. L'unité doit
             être cohérente avec celle de [W.get_time] (secondes pour
             [TimeWeight] et [CorrespondenceWeight], mètres pour
             [DistanceWeight], sauts pour [HopWeight]). Doit être admissible
             (ne jamais surestimer) pour garantir l'optimalité. *)
  let find_path ~start_time ~(graph : G.t) ~(source : G.Node.t)
      ~(destination : G.Node.t) ~(transfer_allowed : G.Node.t -> bool)
      ~(heuristic : G.Node.t -> G.Node.t -> float) =
    if not (G.has_node graph source) then None
    else if not (G.has_node graph destination) then None
    else
      let initial_weight = W.start_at start_time in
      if G.Node.equal source destination then
        Some { P.steps = []; total_weight = initial_weight }
      else
        let dist = Hashtbl.create 10000 in
        let prev = Hashtbl.create 10000 in

        let best_path = ref None in
        let best_weight = ref W.infinity in

        let initial_state = (source, W.get_state_key initial_weight) in
        let g_scalar g = float_of_int (W.get_time g) in
        let initial_f = g_scalar initial_weight +. heuristic source destination in
        
        let pq = ref (PQ.push ~f_cost:initial_f ~g_cost:initial_weight source initial_state PQ.empty) in
        Hashtbl.add dist initial_state initial_weight;

        let rec process () =
          match PQ.pop !pq with
          | None -> !best_path
          | Some (_f_cost, g_cost, u, u_state, rest_pq) ->
              pq := rest_pq;
              let recorded_g = Option.value ~default:W.infinity (Hashtbl.find_opt dist u_state) in
              if W.(g_cost > recorded_g) then process ()
              else if W.(g_cost > !best_weight) then process ()
              else if G.Node.equal u destination then begin
                let rec build_steps acc state =
                  match Hashtbl.find_opt prev state with
                  | None -> acc
                  | Some (pred_state, pred_node, current_node, edge, w, step_data) ->
                      let step =
                        { P.from = pred_node; to_node = current_node; edge; weight = w; data = step_data; }
                      in
                      build_steps (step :: acc) pred_state
                in
                best_path := Some { P.steps = build_steps [] u_state; total_weight = Hashtbl.find dist u_state; };
                best_weight := g_cost;
                process ()
              end else begin
                List.iter
                  (fun (v, edge) ->
                    match W.traverse g_cost edge with
                    | None -> ()
                    | Some (new_g, step_data) ->
                        let next_state_key = W.get_state_key new_g in
                        let _, current_state_key = u_state in
                        let is_line_change = Stdlib.compare current_state_key next_state_key <> 0 in
                        let is_transfer = is_line_change && not (G.Node.equal u source) in
                        
                        if (not is_transfer) || transfer_allowed u then (
                          let v_state = (v, next_state_key) in
                          let old_g = Option.value ~default:W.infinity (Hashtbl.find_opt dist v_state) in

                          if W.(new_g < old_g) && not W.(new_g > !best_weight) then begin
                            let new_f = g_scalar new_g +. heuristic v destination in
                            Hashtbl.replace dist v_state new_g;
                            Hashtbl.replace prev v_state (u_state, u, v, edge, new_g, step_data);
                            pq := PQ.push ~f_cost:new_f ~g_cost:new_g v v_state !pq
                          end))
                  (G.neighbors graph u);
                process ()
              end
        in
        process ()
end