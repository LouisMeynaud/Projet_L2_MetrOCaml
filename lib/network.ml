(** Transport mode enumerated type. *)
type transport_mode =
  | Metro
  | RER
  | Bus
  | Tram
  | Walk
  | Custom of string

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
  line : string;
  duration : int;
  distance : float option;
  mode : transport_mode;
}