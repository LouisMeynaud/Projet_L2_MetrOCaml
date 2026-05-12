val category_of_distance : int -> string
val pick_random_station : 'a list -> 'a option
val pick_random_pair : 'a list -> ('a * 'a) option

val run :
  title:string ->
  resolve:('g -> string -> string -> ('g * 'n * 'n) option) ->
  'g ->
  string ->
  string ->
  init:'a ->
  (string * ('g -> 'n -> 'n -> 'a -> string * (unit -> unit))) list ->
  (string -> string list -> string) ->
  unit

module type Random_Bench = sig
  type graph
  type node
  type routeur_pair
  type weight

  val make_routeur_pairs : graph -> routeur_pair list
  val get_nodes : graph -> node list
  val build_context : graph -> node -> node -> (graph * node * node) option
  val make_start_weight : unit -> weight
  val benchmark_pair :
    routeur_pair list -> graph -> node -> node -> weight ->
    (string * int * int * float * float * float * float) option
end

module MakeRandomBench (RBench : Random_Bench) : sig
  val run_random_bench_console : RBench.graph -> unit
end
