(** Modélisation des noeuds et arêtes pour les réseaux de transport. 
    Ce module fournit les types de base nécessaires à la construction d'un TransportGraph
    et peut servir de fondation pour tout algorithme de routage basé sur les graphes. *)

type line = { name : string; color : string;id : string;}
(** Informations relatives à la ligne. *)

type transport_mode =
  | Metro
  | RER
  | Bus
  | Tram
  | Walk
  | Transfert
  | Custom of string  (** Transport mode enumerated type. *)

let string_of_mode = function
  | Metro -> "Metro"
  | RER -> "RER"
  | Bus -> "Bus"
  | Tram -> "Tram"
  | Walk -> "Walk"
  | Transfert -> "Transfert"
  | Custom s -> s

(** Informations relatives aux stations (noeuds). *)
type station = {
  id : string;
  name : string;
  latitude : float;
  longitude : float;
  pmr : bool;
}

(** Informations relatives aux connexions (arêtes). *)
type connection = {
  line : line;
  duration : int;
  schedules : (string * int) list;
  distance : float;
  mode : transport_mode;
  terminus : station;
}

(** Implémentation du module StationNode pour la bibliothèque de routage.
    Les stations sont les noeuds du TransportGraph. *)
module StationNode : Routing.NODE with type t = station = struct
  type t = station

  let equal a b = String.equal a.id b.id
  let hash s = Hashtbl.hash s.id
  let to_string s = s.name
end

(** Implémentation du module ConnectionEdge pour la bibliothèque de routage.
    Les connexions correspondent aux arêtes du TransportGraph. *)
module ConnectionEdge : Routing.EDGE with type t = connection = struct
  type t = connection
end

(** Une instance de TransportGraph permettant la modélisation de réseaux. *)
module TransportGraph = Routing.Graph.Make (StationNode) (ConnectionEdge)