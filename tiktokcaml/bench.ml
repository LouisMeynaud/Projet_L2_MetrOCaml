let run graphe source_query dest_query start_time routeurs raw_select =
  Routing.Bench.run
    ~title:"Benchmark TikToCaml"
    ~resolve:(fun g src dst -> Graph.resolve_people g src dst)
    graphe source_query dest_query start_time routeurs raw_select
