open Graph_intf
open Weight_intf

(** Dijkstra Optimisé

    Ce module implémente l'algorithme du plus court chemin de Dijkstra via un
    Foncteur. Il a été profondément modifié par rapport à sa version académique
    pour répondre aux contraintes des réseaux de transports en commun (GTFS)
    avec :

    - Découplage Total : Il ignore la logique métier (métro, horaires) et
      délègue l'évaluation au module [WEIGHT].
    - State-Space Routing : Il ne route pas d'une "station" à une autre, mais
      d'un "état" à un autre (Station + Ligne) pour ne pas écraser les chemins
      valides.
    - Pairing Heap : Il utilise un Tas d'Appariement garantissant des
      performances quasi-instantanées, même sur des graphes massifs comme celui
      de Paris. *)
module Make (G : GRAPH) (W : WEIGHT with type edge = G.Edge.t) = struct
  module P = Path.Make (G) (W)

  type path = P.t

  (** Le Tas d'Appariement (Pairing Heap) *)
  module PQ = struct
    (* Il permet de résoudre un soucis récurent : le meilleur moyen d'arriver à une station intermédiaire
       n'est pas forcément celui qui permettra d'arriver à destination le plus vite (notamment à cause
       des correspondances). Pour cela on retient donc toutes les manières d'arriver à cette station pour
       les exploiter également. *)
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

    (* L'opération centrale du Pairing Heap. Pour insérer un élément, on ne parcourt 
       rien. On prend les deux racines, on les compare, et l'arbre perdant 
       devient un simple enfant de l'arbre gagnant. *)
    let merge h1 h2 =
      match (h1, h2) with
      | Empty, h | h, Empty -> h
      | Node n1, Node n2 ->
          if n1.f_cost <= n2.f_cost then
            Node { n1 with children = h2 :: n1.children }
          else Node { n2 with children = h1 :: n2.children }

    let push ~f_cost ~g_cost node state h =
      merge (Node { f_cost; g_cost; node; state; children = [] }) h

    (* Quand on retire la racine (pop), tous ses enfants deviennent orphelins.
       Pour garder un arbre compact, on les fait s'affronter deux par deux. 
       Le motif récursif fusionne de gauche à droite, puis de droite à gauche. *)
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
      @param transfer_allowed Fonction de vérification des transferts. *)
  let find_path ~start_time ~(graph : G.t) ~(source : G.Node.t)
      ~(destination : G.Node.t) ~(transfer_allowed : G.Node.t -> bool) =
    if not (G.has_node graph source) then None
    else if not (G.has_node graph destination) then None
    else
      let initial_weight = W.start_at start_time in
      if G.Node.equal source destination then
        Some { P.steps = []; total_weight = initial_weight }
      else
        (* 'dist' stocke le meilleur poids connu pour un état donné.
           'prev' stocke l'historique (le fil d'Ariane) pour reconstruire le chemin à la fin. *)
        let dist = Hashtbl.create 10000 in
        let prev = Hashtbl.create 10000 in

        let best_path = ref None in
        let best_weight = ref W.infinity in

        let initial_state = (source, W.get_state_key initial_weight) in
        let g_scalar g = float_of_int (W.get_time g) in
        let initial_f = g_scalar initial_weight in
        
        let pq = ref (PQ.push ~f_cost:initial_f ~g_cost:initial_weight source initial_state PQ.empty) in
        Hashtbl.add dist initial_state initial_weight;

        let rec process () =
          match PQ.pop !pq with
          | None -> !best_path (* Le tas est vide, on renvoie le meilleur chemin trouvé (ou None si inatteignable) *)
          | Some (_f_cost, g_cost, u, u_state, rest_pq) ->
              pq := rest_pq;
              let recorded_g = Option.value ~default:W.infinity (Hashtbl.find_opt dist u_state) in
              if W.(g_cost > recorded_g) then process ()
              else if W.(g_cost > !best_weight) then process ()
              else if G.Node.equal u destination then begin
                (* On remonte le fil d'Ariane dans 'prev' pour créer la liste des étapes *)
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
                (* Exploration de toutes les arêtes adjacentes *)
                List.iter
                  (fun (v, edge) ->
                    (* Le module métier (Weight) décide si on a le droit de passer *)
                    match W.traverse g_cost edge with
                    | None -> ()
                    | Some (new_g, step_data) ->
                        let next_state_key = W.get_state_key new_g in
                        let _, current_state_key = u_state in
                        let is_line_change = Stdlib.compare current_state_key next_state_key <> 0 in (*detecte si on a changer de ligne ou pas*)
                        let is_transfer = is_line_change && not (G.Node.equal u source) in (*on considère que le changement de ligne à la station de départ n'est pas une correspondance*)
                        
                        if (not is_transfer) || transfer_allowed u then (
                          let v_state = (v, next_state_key) in
                          let old_g = Option.value ~default:W.infinity (Hashtbl.find_opt dist v_state) in

                          (* Si le passage par cette arête nous offre un meilleur score (new_g < old_g) 
                             que notre précédent record pour cet état, on procède à une "Relaxation" :
                             1. On met à jour le record officiel dans 'dist'.
                             2. On lie ce nouvel état à l'ancien dans 'prev' (avec son step_data).
                             3. On propulse ce nouvel état prometteur dans le tas 'pq'. *)
                          if W.(new_g < old_g) && not W.(new_g > !best_weight) then begin
                            let new_f = g_scalar new_g in
                            Hashtbl.replace dist v_state new_g;
                            Hashtbl.replace prev v_state (u_state, u, v, edge, new_g, step_data);
                            pq := PQ.push ~f_cost:new_f ~g_cost:new_g v v_state !pq
                          end))
                  (G.neighbors graph u);
                  
                (* Boucle récursive : on passe au nœud suivant dans le tas *)
                process ()
              end
        in
        process ()
end