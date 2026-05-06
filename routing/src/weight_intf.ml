(** Interface pour les modules de poids *)
module type WEIGHT = sig
  type t
  type edge

  type node

  type step_data
  (** Données opaques générées lors de la traversée des noeuds (par exemple : les temps d'attente) *)

  type state_key
  (** Clé opaque utilisée pour distinguer les états au niveau d'un même noeud (nom des lignes) *)

  val start_at : int -> t
  (** Initialise le poids avec une heure de départ (en secondes à partir de minuit) *)

  val infinity : t
  (** Valeur correspondant à un coût inaccessible/infini *)

  val is_inf : t -> bool
  (** Vérifie si la valeur est infinie *)

  val traverse : t -> node -> edge -> (t * step_data) option
  (** Tente de parcourir une arête. Renvoie None si cela est impossible (par exemple, s'il n'y a 
      pas de train), sinon, renvoie le nouveau poids *)

  val compare : t -> t -> float
  (** Comparaison de poids. Renvoie une valeur flottante comprise entre [-1, 1] désignant l'amplitude de la différence. *)

  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val get_weight : t -> int
  val to_string : t -> string

  val step_data_to_string : step_data -> string
  (** Convertit les données « step_data » opaques en une chaîne imprimable pour l'interface UI/Show *)

  val get_state_key : t -> state_key
  (** Extrait la clé d'état du poids actuel afin d'empêcher Dijkstra de supprimer prématurément 
      les chemins ayant un contexte différent (par exemple, différentes lignes de métro) *)
end
