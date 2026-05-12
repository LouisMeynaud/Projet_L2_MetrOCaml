open Weight_intf

type ('edge, 'node) criterion

val criterion :
  (module WEIGHT with type edge = 'e and type node = 'n and type t = 'w) ->
  coefficient:float ->
  label:string ->
  ('e, 'n) criterion

val combine :
  ('e, 'n) criterion list ->
  (module WEIGHT with type edge = 'e and type node = 'n)
