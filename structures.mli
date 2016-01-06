module type Field = sig
    type t
    val e_add : t
    val e_mul : t
    val max_field : t
    val ( =. ) : t -> t -> bool
    val ( <>. ) : t -> t -> bool
    val ( >=. ) : t -> t -> bool
    val ( +. ) : t -> t -> t
    val ( -. ) : t -> t -> t
    val ( *. ) : t -> t -> t
    val ( /. ) : t -> t -> t
    val print : t -> unit
end
