open Weight_intf

type ('edge, 'w) criterion_t = {
  weight      : (module WEIGHT with type edge = 'edge and type t = 'w);
  coefficient : float;
  label       : string;
}

type 'edge criterion =
  | Criterion : ('edge, 'w) criterion_t -> 'edge criterion

let criterion
  (type e w) (weight : (module WEIGHT with type edge = e and type t = w))
  ~(coefficient : float)
  ~(label : string)
: e criterion = Criterion { weight; coefficient; label }

type 'edge entry =
  | Entry : ('edge, 'w) criterion_t * 'w -> 'edge entry

  
let combine (type e) (criteria : e criterion list)
  : (module WEIGHT with type edge = e) =
  match criteria with
  | [] -> invalid_arg "Combinator.combine: criteria list must not be empty"
  | _ ->
    let slots = Array.of_list criteria in
    let n = Array.length slots in
    (* Normalise the coefficients to ensure they sum to 1
       NB. Negative coefficients are treated as zero *)
    let total_coeff =
      Array.fold_left
        (fun acc slot -> match slot with Criterion c -> acc +. (Float.max 0.0 c.coefficient))
        0.0 slots
    in
    let norm_coeffs =
      Array.map
        (fun slot -> match slot with Criterion c -> (Float.max 0.0 c.coefficient) /. total_coeff)
        slots
    in
    (module struct
      type t = e entry array
      type edge = e

      let zero =
        Array.init n (fun i ->
          match slots.(i) with
          | Criterion c ->
            let module W = (val c.weight) in
            Entry (c, W.zero))

      let infinity =
        Array.init n (fun i ->
          match slots.(i) with
          | Criterion c ->
            let module W = (val c.weight) in
            Entry (c, W.infinity))

      let of_edge (edge : edge) : t =
        Array.init n (fun i ->
          match slots.(i) with
          | Criterion c ->
            let module W = (val c.weight) in
            Entry (c, W.of_edge edge))

      let is_inf (t : t) : bool =
        Array.exists (fun entry ->
          match entry with
          | Entry (c, v) ->
            let module W = (val c.weight : WEIGHT with type edge = edge and type t = 'w) in
            let v : W.t = Obj.magic v in
            W.is_inf v
        ) t

      let add (a : t) (b : t) : t =
        Array.init n (fun i ->
          match slots.(i), a.(i), b.(i) with
          | Criterion c, Entry (_, va), Entry (_, vb) ->
            let module W = (val c.weight) in
            let va : W.t = Obj.magic va in
            let vb : W.t = Obj.magic vb in
            if W.is_inf va || W.is_inf vb
            then Entry (c, W.infinity)
            else Entry (c, W.add va vb))

      (* Process:
          - For each criterion, compare the corresponding weights from a and b using that criterion's weight module.
          - Multiply the comparison result by the criterion's normalised coefficient.
          - Sum these weighted comparisons to get an overall score.
          - The final comparison is based on this overall score. 
      *)
      let compare (a : t) (b : t) : float =
        if is_inf a && is_inf b then 0.0
        else if is_inf a then 1.0
        else if is_inf b then -1.0
        else
          let sum = ref 0.0 in
          for i = 0 to n - 1 do
            match slots.(i), a.(i), b.(i) with
            | Criterion c, Entry (_, va), Entry (_, vb) ->
              let module W = (val c.weight) in
              let va : W.t = Obj.magic va in
              let vb : W.t = Obj.magic vb in
              sum := !sum +. norm_coeffs.(i) *. W.compare va vb
          done;
          !sum

      let ( < ) a b = compare a b < 0.0
      let ( > ) a b = compare a b > 0.0
      let ( = ) a b = compare a b = 0.0

      let entry_to_string : type w. (edge, w) criterion_t -> w -> string =
        fun c value ->
          let module W = (val c.weight : WEIGHT with type edge = edge and type t = w) in
          Printf.sprintf "%s: %s (x%.4g)"
            c.label
            (W.to_string value)
            c.coefficient

      let to_string (t : t) : string =
        if is_inf t then "∞"
        else
          let parts =
            Array.to_list (Array.map (fun v ->
              match v with
              | Entry (c, value) ->
                entry_to_string c value
            ) t)
          in
          String.concat " | " parts
    end : WEIGHT with type edge = e)
