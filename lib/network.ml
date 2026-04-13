(** This module defines the concepts of nodes and edges in the transport
    context. These types are used to build TransportGraph. It can be used for
    any graph-based routing. *)

type line = { name : string; color : string }
(** Line information. *)

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

type station = {
  id : string;
  name : string;
  latitude : float;
  longitude : float;
  pmr : bool;
}
(** Station (node) information. *)

type connection = {
  line : line;
  duration : int;
  schedules : (string * int) list;
  distance : float;
  mode : transport_mode;
  terminus : station;
}
(** Connection (edge) information. *)

(** Nodes module implementation for the routing library. Stations are the nodes
    in the transport graph. *)
module StationNode : Routing.NODE with type t = station = struct
  type t = station

  let equal a b = String.equal a.id b.id
  let hash s = Hashtbl.hash s.id
  let to_string s = s.name
end

(** Edges module implementation for the routing library. Connections are the
    edges in the transport graph. *)
module ConnectionEdge : Routing.EDGE with type t = connection = struct
  type t = connection
end

module TransportGraph = Routing.Graph.Make (StationNode) (ConnectionEdge)
(** A TransportGraph instance that enables network modelling. *)
