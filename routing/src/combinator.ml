open Weight_intf

type ('edge, 'w) criterion_t = {
  weight : (module WEIGHT with type edge = 'edge and type t = 'w);
  coefficient : float;
  label : string;
}

type 'edge criterion = Criterion : ('edge, 'w) criterion_t -> 'edge criterion

let criterion (type e w)
    (weight : (module WEIGHT with type edge = e and type t = w))
    ~(coefficient : float) ~(label : string) : e criterion =
  Criterion { weight; coefficient; label }

type 'edge entry = Entry : ('edge, 'w) criterion_t * 'w -> 'edge entry

let combine (type e) (criteria : e criterion list) :
    (module WEIGHT with type edge = e) =
  match criteria with
  | [] -> invalid_arg "Combinator.combine: criteria list must not be empty"
  | _ ->
      let slots = Array.of_list criteria in
      let n = Array.length slots in

      let total_coeff =
        Array.fold_left
          (fun acc slot ->
            match slot with Criterion c -> acc +. Float.max 0.0 c.coefficient)
          0.0 slots
      in
      let norm_coeffs =
        Array.map
          (fun slot ->
            match slot with
            | Criterion c -> Float.max 0.0 c.coefficient /. total_coeff)
          slots
      in

      (module struct
        type t = e entry array
        type edge = e
        type step_data = string
        type state_key = Obj.t array

        let start_at time =
          Array.init n (fun i ->
              match slots.(i) with
              | Criterion c ->
                  let module W = (val c.weight) in
                  Entry (c, W.start_at time))

        let infinity =
          Array.init n (fun i ->
              match slots.(i) with
              | Criterion c ->
                  let module W = (val c.weight) in
                  Entry (c, W.infinity))

        let is_inf (t : t) : bool =
          Array.exists
            (fun entry ->
              match entry with
              | Entry (c, v) ->
                  let module W =
                    (val c.weight
                        : WEIGHT with type edge = edge and type t = 'w)
                  in
                  let v : W.t = Obj.magic v in
                  W.is_inf v)
            t

        let traverse (a : t) (e : edge) : (t * step_data) option =
          let new_states = Array.copy a in
          let step_str = ref "" in
          let possible = ref true in
          for i = 0 to n - 1 do
            if !possible then
              match (slots.(i), a.(i)) with
              | Criterion c, Entry (_, va) -> (
                  let module W = (val c.weight) in
                  let va_typed : W.t = Obj.magic va in
                  if W.is_inf va_typed then
                    new_states.(i) <- Entry (c, W.infinity)
                  else
                    match W.traverse va_typed e with
                    | None -> possible := false
                    | Some (new_va, s_data) ->
                        if i = 0 then step_str := W.step_data_to_string s_data;
                        new_states.(i) <- Entry (c, new_va))
          done;
          if !possible then Some (new_states, !step_str) else None

        let compare (a : t) (b : t) : float =
          if is_inf a && is_inf b then 0.0
          else if is_inf a then 1.0
          else if is_inf b then -1.0
          else
            let sum = ref 0.0 in
            for i = 0 to n - 1 do
              match (slots.(i), a.(i), b.(i)) with
              | Criterion c, Entry (_, va), Entry (_, vb) ->
                  let module W = (val c.weight) in
                  let va : W.t = Obj.magic va in
                  let vb : W.t = Obj.magic vb in
                  sum := !sum +. (norm_coeffs.(i) *. W.compare va vb)
            done;
            !sum

        let ( < ) a b = compare a b < 0.0
        let ( > ) a b = compare a b > 0.0
        let ( = ) a b = compare a b = 0.0

        let get_time (t : t) : int =
          match t.(0) with
          | Entry (c, v) ->
              let module W = (val c.weight) in
              W.get_time (Obj.magic v)

        let entry_to_string : type w. (edge, w) criterion_t -> w -> string =
         fun c value ->
          let module W =
            (val c.weight : WEIGHT with type edge = edge and type t = w)
          in
          Printf.sprintf "%s: %s (x%.4g)" c.label (W.to_string value)
            c.coefficient

        let to_string (t : t) : string =
          if is_inf t then "∞"
          else
            let parts =
              Array.to_list
                (Array.map
                   (fun v ->
                     match v with Entry (c, value) -> entry_to_string c value)
                   t)
            in
            String.concat " | " parts

        let step_data_to_string (s : step_data) = s

        let get_state_key (t : t) : state_key =
          Array.init n (fun i ->
              match t.(i) with
              | Entry (c, v) ->
                  let module W = (val c.weight) in
                  Obj.repr (W.get_state_key (Obj.magic v)))
      end : WEIGHT
        with type edge = e)
