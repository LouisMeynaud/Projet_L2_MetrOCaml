(**Path module signature *)

module type PATH = sig
  type node
  type edge
  type weight

  type step = { from : node; to_node : node; edge : edge; weight : weight }
  (** A single step in a path, representing a traversal from one node to another
      containing a source, destination edge and weight*)

  type t = { steps : step list; total_weight : weight }
  (** List of all steps from source to destination*)
end
