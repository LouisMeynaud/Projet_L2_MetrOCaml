open Graph_intf
open Weight_intf

module type PATH = sig
  module Node : NODE

  type node = Node.t

  module Edge : EDGE

  type edge = Edge.t

  module Weight : WEIGHT

  type weight = Weight.t

  type step_data = Weight.step_data

  type step = {
    from : node;
    to_node : node;
    edge : edge;
    weight : weight;
    data : step_data;
  }
  (** Une seule étape dans le chemin, représentant le passage d'un noeud à un autre et contenant
   la station source, la station destination, la connexion entre les deux, le poids et une donnée secondaire. *)

  type t = { steps : step list; total_weight : weight }
  (** Liste de toutes les étapes de la station source à la station destination. *)
end
