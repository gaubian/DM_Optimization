module Make(F : Structures.Field) = struct
    module IntMap = Map.Make(Ex_structures.Int)

    type t = (F.t IntMap.t) IntMap.t

    let empty : t =
        IntMap.empty

    let nb_v : t -> int =
        IntMap.cardinal

    let mem_v : t -> int -> bool =
        fun grph i ->
            IntMap.mem i grph

    let add_v : t -> int -> t =
        fun grph i ->
            IntMap.add i (IntMap.empty) grph

    let del_v : t -> int -> t =
        fun grph i ->
            IntMap.remove i grph

    let adjac : t -> int -> F.t IntMap.t =
        fun grph i ->
            IntMap.find i grph

    let mem_e : t -> int -> int -> bool =
        fun grph i j ->
            IntMap.mem j (adjac grph i)

    let find_e : t -> int -> int -> F.t =
        fun grph i j ->
            IntMap.find j (adjac grph i)

    let add_e : t -> int -> int -> F.t -> t =
       fun grph i j x ->
          let ad_i = adjac grph i and ad_j = adjac grph j in
          let nw_i = IntMap.add j x ad_i and nw_j = IntMap.add i x ad_j in
              IntMap.add 

end
