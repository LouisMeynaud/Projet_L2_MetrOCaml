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

  let is_inf = ( = ) infinity

  let add = ( + )

  (** (a - b) / max(a, b)*)
  let compare a b =
    if a = infinity && b = infinity then 0.0
    else if a = infinity then 1.0
    else if b = infinity then -1.0
    else
      let diff = a - b in
      let denom = Int.max 1 (Int.max a b) in
      float_of_int diff /. float_of_int denom

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0

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

  let is_inf = ( = ) infinity

  let add = ( +. )

  (** (a - b) / max(a, b)*)
  let compare a b =
    if a = infinity && b = infinity then 0.0
    else if a = infinity then 1.0
    else if b = infinity then -1.0
    else
      let diff = a -. b in
      let denom = Float.max 1.0 (Float.max a b) in
      diff /. denom

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0

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

  let is_inf = ( = ) infinity

  let add w1 w2 =
    if w1 = infinity || w2 = infinity then infinity
    else w1 + w2

  (** (a - b) / max(a, b)*)
  let compare a b =
    if a = infinity && b = infinity then 0.0
    else if a = infinity then 1.0
    else if b = infinity then -1.0
    else
      let diff = a - b in
      let denom = Int.max 1 (Int.max a b) in
      float_of_int diff /. float_of_int denom

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0

  let to_string w =
    if w = infinity then "∞"
    else if w = 1 then "1 hop"
    else Printf.sprintf "%d hops" w
end


(** Module de calcul de poids basé sur le nombre de correspondances.
    
Ce module implémente l'interface [Routing.WEIGHT]. Il permet d'optimiser les trajets 
en minimisant les changements de ligne. *)
module CorrespondenceWeight : Routing.WEIGHT with type edge = connection = struct

  (** Le type [t] est un poids composite :
    - [int] : Le nombre total de correspondances.
    - [string option] : Le nom de la dernière ligne empruntée pour détecter les changements. *)
  type t = int * string option
  type edge = connection
    
  (** Poids initial : aucune correspondance et aucune ligne enregistrée. *)
  let zero = (0, None)
    
  (** Poids infini utilisé pour les nœuds non atteignables. *)
  let infinity = (max_int, None)
    
  (** Constante de pénalité : 1 ajouté à chaque correspondance. *)
  let transfer_penalty = 1
    
  (** Transforme une arête du réseau en poids.
  Extrait le nom de la ligne de la structure [connection]. *)
  let of_edge (conn : connection) = 
    ( 0 , Some conn.line.name)
    
  let is_inf (weight : t) =
    let (correspondance, _) = weight in
      correspondance = max_int

  (** Calcule la somme de deux poids.
  Détecte si une correspondance a lieu en comparant la ligne du poids cumulé 
  avec celle de la nouvelle arête. 
  @return Un nouveau poids incluant le nombre de correspondance avec l'éventuelle pénalité. *)
  let add (total_correspondance, last_line) (edge_correspondance, current_line) =
    if total_correspondance = max_int then infinity
    else
      let penalty = match last_line, current_line with
        | Some l1, Some l2 when l1 <> l2 -> transfer_penalty
        | _ -> 0 (* Pas de pénalité au départ ou si la ligne est identique *)
      in
        (total_correspondance + edge_correspondance + penalty, current_line)
    
  (** Compare deux poids en se basant uniquement sur la composante temporelle.
  Nécessaire pour le fonctionnement de l'algorithme de Dijkstra. *)
  (** (a - b) / max(a, b)*)
  let compare (t1, _) (t2, _) =
    if t1 = max_int && t2 = max_int then 0.0
    else if t1 = max_int then 1.0
    else if t2 = max_int then -1.0
    else
      let diff = t1 - t2 in
      let denom = Int.max 1 (Int.max t1 t2) in
      float_of_int diff /. float_of_int denom
    
  (* Opérateurs de comparaison spécialisés pour le type [t]. *)
  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
    
  (** Convertit le poids en une chaîne de caractères lisible.
  Affiche "∞" pour le poids infini. *)
  let to_string (weight : t) =
    let (correspondance, _) = weight in
      if Stdlib.(=) correspondance max_int then "∞"
      else 
        Printf.sprintf "%d correspondance(s)" correspondance
end