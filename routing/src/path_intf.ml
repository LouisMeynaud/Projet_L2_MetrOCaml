(**Path module signature *)

open Graph_intf
open Weight_intf

module type PATH = sig
  module Node : NODE

  type node = Node.t

  module Edge : EDGE

  type edge = Edge.t

  module Weight : WEIGHT

  type weight = Weight.t

  type step = { from : node; to_node : node; edge : edge; weight : weight }
  (** A single step in a path, representing a traversal from one node to another
      containing a source, destination edge and weight*)

  type t = { steps : step list; total_weight : weight }
  (** List of all steps from source to destination*)
end
