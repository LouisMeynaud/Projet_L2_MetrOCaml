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
    type t = Empty | Node of W.t * G.Node.t * state_tuple * t list

    let empty = Empty

    (* L'opération centrale du Pairing Heap. Pour insérer un élément, on ne parcourt 
       rien. On prend les deux racines, on les compare, et l'arbre perdant 
       devient un simple enfant de l'arbre gagnant. *)
    let merge h1 h2 =
      match (h1, h2) with
      | Empty, h | h, Empty -> h
      | Node (w1, n1, s1, l1), Node (w2, n2, s2, l2) ->
          if W.(w1 < w2) then Node (w1, n1, s1, h2 :: l1)
          else Node (w2, n2, s2, h1 :: l2)

    let push w n s h = merge (Node (w, n, s, [])) h

    (* Quand on retire la racine (pop), tous ses enfants deviennent orphelins.
       Pour garder un arbre compact, on les fait s'affronter deux par deux. 
       Le motif récursif fusionne de gauche à droite, puis de droite à gauche. *)
    let rec merge_pairs = function
      | [] -> Empty
      | [ h ] -> h
      | h1 :: h2 :: rest -> merge (merge h1 h2) (merge_pairs rest)

    let pop = function
      | Empty -> None
      | Node (w, n, s, l) -> Some (w, n, s, merge_pairs l)
  end

  (*restriction pmr false par defaut , et fonction renvoir true par defaut *)
  let pmr_transfer = ref false
  let station_is_pmr = ref (fun _ -> true)
  (* on active les restriction pmr si voulu , et on donne la fonction de vérification*)
  let set_pmr_transfer ~pmr_transfer:enabled ~station_is_pmr:is_pmr =
    pmr_transfer := enabled;
    station_is_pmr := is_pmr

  (** Trouve le chemin optimal de la [source] à la [destination].
      @param start_time L'heure de départ (sert de poids initial).
      @param graph Le graphe pré-construit du réseau. *)
  let find_path ~start_time ~graph ~source ~destination =
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

        let initial_state = (source, W.get_state_key initial_weight) in
        let pq = ref (PQ.push initial_weight source initial_state PQ.empty) in
        Hashtbl.add dist initial_state initial_weight;

        let rec process () =
          match PQ.pop !pq with
          | None -> None (* Le tas est vide, la destination est inatteignable *)
          | Some (d, u, u_state, rest_pq) ->
              pq := rest_pq;

              if G.Node.equal u destination then
                (* On remonte le fil d'Ariane dans 'prev' pour créer la liste des étapes *)
                let rec build_steps acc state =
                  match Hashtbl.find_opt prev state with
                  | None -> acc
                  | Some
                      (pred_state, pred_node, current_node, edge, w, step_data)
                    ->
                      let step =
                        {
                          P.from = pred_node;
                          to_node = current_node;
                          edge;
                          weight = w;
                          data = step_data;
                        }
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
                (* Exploration de toutes les arêtes adjacentes *)
                List.iter
                  (fun (v, edge) ->
                    (* Le module métier (Weight) décide si on a le droit de passer *)
                    match W.traverse d edge with
                    | None -> ()
                    | Some (new_dist, step_data) ->
                        let next_state_key = W.get_state_key new_dist in
                        let _, current_state_key = u_state in
                        let is_line_change = Stdlib.compare current_state_key next_state_key <> 0 in (*detecte si on a changer de ligne ou pas*)
                        let is_transfer = is_line_change && not (G.Node.equal u source) in (*on considère que le changement de ligne à la station de départ n'est pas une correspondance*)
                        let transfer_allowed = (not !pmr_transfer) || (not is_transfer) || (!station_is_pmr) u in
                        if transfer_allowed then (
                          let v_state = (v, next_state_key) in
                          let old_dist =
                            Option.value ~default:W.infinity
                              (Hashtbl.find_opt dist v_state)
                          in

                          (* Si le passage par cette arête nous offre un meilleur score (new_dist < old_dist) 
                             que notre précédent record pour cet état, on procède à une "Relaxation" :
                             1. On met à jour le record officiel dans 'dist'.
                             2. On lie ce nouvel état à l'ancien dans 'prev' (avec son step_data).
                             3. On propulse ce nouvel état prometteur dans le tas 'pq'. *)
                          if W.(new_dist < old_dist) then begin
                            Hashtbl.replace dist v_state new_dist;
                            Hashtbl.replace prev v_state
                              (u_state, u, v, edge, new_dist, step_data);
                            pq := PQ.push new_dist v v_state !pq
                          end))
                  (G.neighbors graph u);

                (* Boucle récursive : on passe au nœud suivant dans le tas *)
                process ()
              end
        in
        process ()
end
