module type Field = sig
    type t
    val e_add : t
    val e_mul : t
    val max_field : t
    val compare : t -> t -> int
    val ( =. ) : t -> t -> bool
    val ( <>. ) : t -> t -> bool
    val ( >=. ) : t -> t -> bool
    val ( +. ) : t -> t -> t
    val ( -. ) : t -> t -> t
    val ( *. ) : t -> t -> t
    val ( /. ) : t -> t -> t
    val sqrt : t -> t
    val print : out_channel -> t -> unit
    val scan : in_channel -> t
end
