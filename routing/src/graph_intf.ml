(** Interface générique pour les graphes. Ce module définit l'interface abstraite pour les graphes, noeuds et arêtes.
  Il est complètement indépendant du contexte d'utilisation et peut être utilisé pour tout routage basé sur des graphes. *)

(** Abstraction des noeuds. Les données de noeud sont opaques à la structure du graphe. *)
module type NODE = sig
  type t

  val equal : t -> t -> bool
  (** Égalité entre deux noeuds. *)

  val hash : t -> int
  (** Fonction de hachage pour les tables de hachage. *)
end


(** Abstraction des arêtes. Les données d'arête sont opaques à la structure du graphe. *)
module type EDGE = sig
  type t
end


(** Abstraction du graphe. Représente un graphe dirigé avec des noeuds et des arêtes typés.
    Paramétré par le type de noeud et le type d'arête. *)
module type GRAPH = sig
  module Node : NODE
  type node = Node.t

  module Edge : EDGE
  type edge = Edge.t

  type t

  val create : unit -> t
  (** Crée un graphe vide. *)

  val nodes : t -> node list
  (** [nodes g] renvoie tous les noeuds du graphe. *)

  val has_node : t -> node -> bool
  (** [has_node g n] vérifie si le noeud [n] existe dans le graphe. *)

  val add_node : t -> node -> unit
  (** [add_node g n] ajoute un noeud [n] au graphe. *)

  val add_edge : t -> node -> node -> edge -> unit
  (** [add_edge g src dst data] ajoute une arête dirigée de [src] à [dst]. *)

   val size : t -> int
  (** [size g] renvoie le nombre de noeuds dans le graphe en temps constant. *)

  val neighbors : t -> node -> (node * edge) list
  (** [neighbors g n] renvoie tous les noeuds accessibles à partir de [n] ainsi que l'arête associée. *)
end
