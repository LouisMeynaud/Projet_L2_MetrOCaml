(** Interface d'une heuristique pour A*. L'estimation doit être admissible
    (ne jamais surestimer le coût réel restant) pour garantir l'optimalité. *)
module type HEURISTIC = sig
  type node
  val estimate : node -> node -> float
end
