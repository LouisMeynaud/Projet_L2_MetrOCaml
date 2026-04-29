module type S = sig
  module P : Path_intf.PATH
  type graph

  val find_path :
    start_time:int ->
    graph:graph ->
    source:P.Node.t ->
    destination:P.Node.t ->
    transfer_allowed:(P.Node.t -> bool) ->
    P.t option
end
