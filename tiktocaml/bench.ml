let run graphe source_query dest_query ~init routeurs raw_select =
  Routing.Bench.run
    ~title:"Benchmark TikToCaml"
    ~resolve:(fun g src dst -> Graph.resolve_people g src dst)
    graphe source_query dest_query ~init routeurs raw_select
