open Metrocaml

let run graphe source_query dest_query start_time routeurs raw_select =
  Routing.Bench.run
    ~title:"Benchmark"
    ~resolve:(fun g src dst -> Endpoint.build_routing_context g src dst)
    graphe source_query dest_query start_time routeurs raw_select