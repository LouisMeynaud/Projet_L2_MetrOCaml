open Graph_intf
open Weight_intf

(** Interface générique pour les chemins. *)
module type PATH = sig
  module Node : NODE
  type node = Node.t

  module Edge : EDGE
  type edge = Edge.t

  module Weight : WEIGHT
  type weight = Weight.t

  type step = {
    from : node;
    to_node : node;
    edge : edge;
    weight : weight;
  }
  (** Une étape du chemin : trajet entre deux noeuds *)

  type t = { steps : step list; total_weight : weight }
  (** Un chemin : liste d'étapes avec un poids total *)
end
