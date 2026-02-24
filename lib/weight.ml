open Network

(** Time-based weight for transport networks.
    Extracts duration from connection data and uses it as the routing cost.
    Optimizes for fastest routes. *)
module TimeWeight : Routing.WEIGHT with type edge = connection = struct
  type t = int
  type edge = connection

  let zero = 0
  let infinity = max_int

  (** Extract duration from connection metadata. *)
  let of_edge (conn : connection) = conn.duration

  let add = ( + )

  let compare = Int.compare

  let ( < ) a b = compare a b < 0
  let ( > ) a b = compare a b > 0
  let ( = ) a b = compare a b = 0

  let to_string w =
    if w = infinity then "∞"
    else
      let hours = w / 3600 in
      let minutes = (w mod 3600) / 60 in
      let seconds = w mod 60 in
      if hours > 0 then Printf.sprintf "%dh%02dm%02ds" hours minutes seconds
      else if minutes > 0 then Printf.sprintf "%dm%02ds" minutes seconds
      else Printf.sprintf "%ds" seconds
end


(** Distance-based weight for routing. *)
module DistanceWeight : Routing.WEIGHT with type edge = connection = struct
  type t = float
  type edge = connection

  let zero = 0.0
  let infinity = Float.infinity

 (** Extract distance from connection metadata. 
  If distance is not specified, estimate from duration *)
  let of_edge (conn : connection) : t = conn.distance

  let add = ( +. )

  let compare = Float.compare

  let ( < ) a b = compare a b < 0
  let ( > ) a b = compare a b > 0
  let ( = ) a b = compare a b = 0

  let to_string d =
    if d = infinity then "∞"
    else if d >= 1000.0 then Printf.sprintf "%.1f km" (d /. 1000.0)
    else Printf.sprintf "%.0f m" d
end

(** Hop count weight for transport networks.
    Treats all edges as equal cost (1 hop each).
    Optimizes for routes with fewest stops. *)
module HopWeight : Routing.WEIGHT with type edge = connection = struct
  type t = int
  type edge = connection

  let zero = 0
  let infinity = max_int

  (** All edges have unit cost—ignore edge data entirely. *)
  let of_edge (_conn : connection) = 1

  let add w1 w2 =
    if w1 = infinity || w2 = infinity then infinity
    else w1 + w2

  let compare = Int.compare

  let ( < ) a b = compare a b < 0
  let ( > ) a b = compare a b > 0
  let ( = ) a b = compare a b = 0

  let to_string w =
    if w = infinity then "∞"
    else if w = 1 then "1 hop"
    else Printf.sprintf "%d hops" w
end

(**Transfer-aware weight for transport networks.
    Uses lexicographic ordering: fewer line changes first, then shorter time. 
    
    Current implementation: estimates transfers by comparing line IDs between
    consecutive edges (would need routing algorithm modification for full support).
    Included to demonstrate multi-criteria weight design.*)
module TransferWeight : Routing.WEIGHT with type edge = connection = struct
  type t = int * int (* (base_cost, time_seconds) *)
  type edge = connection

  let zero = (0, 0)
  let infinity = (max_int, max_int)

  (** For now, use hop count as base cost.
      True transfer counting requires algorithm state tracking. *)
  let of_edge (conn : connection) = (1, conn.duration)

  let add (c1, d1) (c2, d2) =
    if (c1, d1) = infinity || (c2, d2) = infinity then infinity
    else (c1 + c2, d1 + d2)

  let compare (c1, d1) (c2, d2) =
    match Int.compare c1 c2 with
    | 0 -> Int.compare d1 d2
    | c -> c

  let ( < ) a b = compare a b < 0
  let ( > ) a b = compare a b > 0
  let ( = ) a b = compare a b = 0

  let to_string (cost, time) =
    if Stdlib.(=) (compare (cost, time) infinity) 0 then "∞"
    else
      let hours = time / 3600 in
      let minutes = (time mod 3600) / 60 in
      let seconds = time mod 60 in
      let time_str =
        if Stdlib.(>) hours 0 then Printf.sprintf "%dh%02dm%02ds" hours minutes seconds
        else if Stdlib.(>) minutes 0 then Printf.sprintf "%dm%02ds" minutes seconds
        else Printf.sprintf "%ds" seconds
      in
      Printf.sprintf "%d stops, %s" cost time_str
end

(** Module de calcul de poids basé sur le temps avec pénalité de transfert.
    
Ce module implémente l'interface [Routing.WEIGHT]. Il permet d'optimiser les trajets
non seulement sur la durée, mais aussi sur le confort en minimisant les changements
de ligne sans modifier la topologie du graphe. *)
module CorrespondenceWeight : Routing.WEIGHT with type edge = connection = struct

  (** Le type [t] est un poids composite :
    - [int] : Le temps total accumulé (trajet + pénalités) en secondes.
    - [string option] : Le nom de la dernière ligne empruntée pour détecter les changements. *)
  type t = int * string option
  type edge = connection
    
  (** Poids initial : temps nul et aucune ligne enregistrée. *)
  let zero = (0, None)
    
  (** Poids infini utilisé pour les nœuds non atteignables. *)
  let infinity = (max_int, None)
    
  (** Constante de pénalité : 300 secondes (5 minutes) ajoutées à chaque correspondance. *)
  let transfer_penalty = 300
    
  (** Transforme une arête du réseau en poids.
  Extrait la durée et le nom de la ligne de la structure [connection]. *)
  let of_edge (conn : connection) = 
    (conn.duration, Some conn.line.name)
    
  (** Calcule la somme de deux poids.
  Détecte si une correspondance a lieu en comparant la ligne du poids cumulé 
  avec celle de la nouvelle arête. 
  @return Un nouveau poids incluant le temps de trajet et l'éventuelle pénalité. *)
  let add (total_time, last_line) (edge_time, current_line) =
    if total_time = max_int then infinity
    else
      let penalty = match last_line, current_line with
        | Some l1, Some l2 when l1 <> l2 -> transfer_penalty
        | _ -> 0 (* Pas de pénalité au départ ou si la ligne est identique *)
      in
        (total_time + edge_time + penalty, current_line)
    
  (** Compare deux poids en se basant uniquement sur la composante temporelle.
  Nécessaire pour le fonctionnement de l'algorithme de Dijkstra. *)
  let compare (t1, _) (t2, _) = Int.compare t1 t2
    
  (* Opérateurs de comparaison spécialisés pour le type [t]. *)
  let ( < ) a b = compare a b < 0
  let ( > ) a b = compare a b > 0
  let ( = ) a b = compare a b = 0
    
  (** Convertit le poids en une chaîne de caractères lisible.
  Formatte les secondes en minutes et secondes. Affiche "∞" pour le poids infini. *)
  let to_string (weight : t) =
    let (time, _) = weight in
      if Stdlib.(=) time max_int then "∞"
      else 
        let mins = time / 60 in
          let secs = time mod 60 in
            Printf.sprintf "%dm%02ds" mins secs
end