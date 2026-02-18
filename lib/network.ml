(* gtfs_adapter.ml *)


(** This module defines the concepts of nodes and edges in the transport context.
    These types are used to build TransportGraph.
    It can be used for any graph-based routing. *)

(** Line information. *)
type line = {
  name : string;
  color : string;
}

(** Transport mode enumerated type. *)
type transport_mode =
  | Metro
  | RER
  | Bus
  | Tram
  | Walk
  | Custom of string

let string_of_mode = function
  | Metro -> "Metro"
  | RER -> "RER"
  | Bus -> "Bus"
  | Tram -> "Tram"
  | Walk -> "Walk"
  | Custom s -> s

(** Station (node) information. *)
type station = {
  id : string;
  name : string;
  latitude : float;
  longitude : float;
  pmr : bool;
}

(** Connection (edge) information. *)
type connection = {
  line : line;
  duration : int;
  distance : float;
  mode : transport_mode;
}

(** Nodes module implementation for the routing library. Stations are the nodes in the transport graph. *)
module StationNode : Routing.NODE with type t = station = struct
  type t = station
  let equal a b = String.equal a.id b.id
  let hash s = Hashtbl.hash s.id
  let to_string s = s.name
end

(** Edges module implementation for the routing library. Connections are the edges in the transport graph. *)
module ConnectionEdge : Routing.EDGE with type t = connection = struct
  type t = connection
end

(** A TransportGraph instance that enables network modelling. *)
module TransportGraph = Routing.Graph.Make(StationNode)(ConnectionEdge)
