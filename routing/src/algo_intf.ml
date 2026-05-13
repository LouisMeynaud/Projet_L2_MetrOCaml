(** Interface générale pour les algorithmes de recherche de chemin *)
module type ALGO = sig
  module P : Path.PATH
  type graph

  val find_path :
    init:P.Weight.t ->
    graph:graph ->
    source:P.Node.t ->
    destination:P.Node.t ->
    can_change_state:(P.Node.t -> bool) ->
    P.t option
end
