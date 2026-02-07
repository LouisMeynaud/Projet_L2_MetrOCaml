module type weight_intf = sig
  type t
  val infinity : t

  val add : t -> t -> t
  val compare : t -> t -> int

  val ( < ) : t -> t -> bool

  val ( > ) : t -> t -> bool

  val ( = ) : t -> t -> bool

  val to_string : t -> string

end